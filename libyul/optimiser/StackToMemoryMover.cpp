/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
#include <libyul/optimiser/StackToMemoryMover.h>
#include <libyul/optimiser/FunctionDefinitionCollector.h>
#include <libyul/optimiser/NameDispenser.h>
#include <libyul/backends/evm/EVMDialect.h>

#include <libyul/AST.h>

#include <libsolutil/CommonData.h>
#include <libsolutil/Visitor.h>

#include <boost/range/adaptors.hpp>

using namespace std;
using namespace solidity;
using namespace solidity::yul;

namespace
{
vector<Statement> generateMemoryStore(
	Dialect const& _dialect,
	langutil::SourceLocation const& _loc,
	YulString _mpos,
	Expression _value
)
{
	BuiltinFunction const* memoryStoreFunction = _dialect.memoryStoreFunction(_dialect.defaultType);
	yulAssert(memoryStoreFunction, "");
	vector<Statement> result;
	result.emplace_back(ExpressionStatement{_loc, FunctionCall{
		_loc,
		Identifier{_loc, memoryStoreFunction->name},
		{
			Literal{_loc, LiteralKind::Number, _mpos, {}},
			std::move(_value)
		}
	}});
	return result;
}

FunctionCall generateMemoryLoad(Dialect const& _dialect, langutil::SourceLocation const& _loc, YulString _mpos)
{
	BuiltinFunction const* memoryLoadFunction = _dialect.memoryLoadFunction(_dialect.defaultType);
	yulAssert(memoryLoadFunction, "");
	return FunctionCall{
		_loc,
		Identifier{_loc, memoryLoadFunction->name}, {
			Literal{
				_loc,
				LiteralKind::Number,
				_mpos,
				{}
			}
		}
	};
}
}

void StackToMemoryMover::run(
	OptimiserStepContext& _context,
	u256 _reservedMemory,
	map<YulString, uint64_t> const& _memorySlots,
	uint64_t _numRequiredSlots,
	Block& _block
)
{
	VariableMemoryOffsetTracker memoryOffsetTracker(_reservedMemory, _memorySlots, _numRequiredSlots);
	list<Statement> newFunctionDefinitions;
	map<YulString, FunctionMoveInfo> functionMoveInfos;
	for (auto const& [function, functionDefinition]: FunctionDefinitionCollector::run(_block))
	{
		FunctionMoveInfo& moveInfo = functionMoveInfos[function];

		size_t argumentCount = functionDefinition->returnVariables.size() + functionDefinition->parameters.size();

		// If the function has only one return variable, it may be called in complex expressions.
		// In this case we always keep the single return variable inside the function.
		// Otherwise we move all return variables that have slots to memory from right to left.
		if (functionDefinition->returnVariables.size() > 1)
		{
			for (TypedName const& returnVariable: functionDefinition->returnVariables | boost::adaptors::reversed)
				if (argumentCount > 16)
				{
					if (auto slot = memoryOffsetTracker(returnVariable.name))
					{
						--argumentCount;
						moveInfo.returnVariableSlots.emplace_back(*slot);
					}
					else
						break;
				}
			if (!moveInfo.returnVariableSlots.empty())
			{
				while (moveInfo.returnVariableSlots.size() < functionDefinition->returnVariables.size())
					moveInfo.returnVariableSlots.emplace_back(nullopt);
				reverse(moveInfo.returnVariableSlots.begin(), moveInfo.returnVariableSlots.end());
			}
		}


		FunctionDefinition* currentHelperFunction = nullptr;
		for (TypedName const& param: functionDefinition->parameters | boost::adaptors::reversed)
		{
			if (argumentCount <= 16)
				break;
			auto slot = memoryOffsetTracker(param.name);
			yulAssert(slot, "Impossible variable memory slot assignment.");
			if (!currentHelperFunction)
			{
				YulString inName = _context.dispenser.newName("in"_yulstring);
				YulString outName = _context.dispenser.newName("out"_yulstring);

				// TODO: rethink source locations
				currentHelperFunction = get_if<FunctionDefinition>(
					&newFunctionDefinitions.emplace_back(FunctionDefinition{
						functionDefinition->location,
						_context.dispenser.newName(functionDefinition->name),
						{TypedName{functionDefinition->location, inName, {}}},
						{TypedName{functionDefinition->location, outName, {}}},
						Block{functionDefinition->location, {}}
					})
				);
				moveInfo.parameterHelperFunctions.emplace_back(currentHelperFunction);
				currentHelperFunction->body.statements.emplace_back(Assignment{
					functionDefinition->location,
						{Identifier{functionDefinition->location, outName}},
						std::make_unique<Expression>(Identifier{functionDefinition->location, inName})
				});
			}
			currentHelperFunction->parameters.insert(currentHelperFunction->parameters.begin() + 1, param);
			currentHelperFunction->body.statements += generateMemoryStore(
				_context.dialect,
				param.location,
				*slot,
				Identifier{param.location, param.name}
			);
			--argumentCount;
			if (currentHelperFunction->parameters.size() == 15)
				currentHelperFunction = nullptr;
		}
	}
	StackToMemoryMover stackToMemoryMover(_context, memoryOffsetTracker, move(functionMoveInfos));
	stackToMemoryMover(_block);
	_block.statements += move(newFunctionDefinitions);
}

StackToMemoryMover::StackToMemoryMover(
	OptimiserStepContext& _context,
	VariableMemoryOffsetTracker const& _memoryOffsetTracker,
	map<YulString, FunctionMoveInfo> const& _functionMoveInfo
):
m_context(_context),
m_memoryOffsetTracker(_memoryOffsetTracker),
m_nameDispenser(_context.dispenser),
m_functionMoveInfo(_functionMoveInfo)
{
	auto const* evmDialect = dynamic_cast<EVMDialect const*>(&_context.dialect);
	yulAssert(
		evmDialect && evmDialect->providesObjectAccess(),
		"StackToMemoryMover can only be run on objects using the EVMDialect with object access."
	);
}

void StackToMemoryMover::operator()(FunctionCall& _functionCall)
{
	ASTModifier::operator()(_functionCall);
	if (!m_functionMoveInfo.count(_functionCall.functionName.name))
		return;

	yulAssert(!m_slotsForCurrentReturns, "");

	FunctionMoveInfo const& moveInfo = m_functionMoveInfo.at(_functionCall.functionName.name);

	for (FunctionDefinition const* helperFunction: moveInfo.parameterHelperFunctions)
	{
		FunctionCall subFunctionCall{
			_functionCall.location,
			Identifier{_functionCall.location, helperFunction->name},
			{}
		};
		for (size_t i = 0; i < helperFunction->parameters.size(); ++i)
		{
			yulAssert(!_functionCall.arguments.empty(), "");
			subFunctionCall.arguments.emplace_back(move(_functionCall.arguments.back()));
			_functionCall.arguments.pop_back();
		}
		reverse(subFunctionCall.arguments.begin(), subFunctionCall.arguments.end());
		_functionCall.arguments.emplace_back(move(subFunctionCall));
	}
	if (!moveInfo.returnVariableSlots.empty())
		m_slotsForCurrentReturns = &moveInfo.returnVariableSlots;
}

void StackToMemoryMover::operator()(FunctionDefinition& _functionDefinition)
{
	// It is important to first visit the function body, so that it doesn't replace the memory inits for
	// variable arguments we might generate below.
	ASTModifier::operator()(_functionDefinition);

	if (!m_functionMoveInfo.count(_functionDefinition.name))
		return;

	FunctionMoveInfo const& moveInfo = m_functionMoveInfo.at(_functionDefinition.name);

	for (FunctionDefinition const* helperFunction: moveInfo.parameterHelperFunctions)
	{
		yulAssert(helperFunction && helperFunction->parameters.size() >= 1, "");
		for (size_t i = 0; i < helperFunction->parameters.size() - 1; ++i)
		{
			yulAssert(!_functionDefinition.parameters.empty(), "");
			_functionDefinition.parameters.pop_back();
		}
	}

	vector<Statement> memoryVariableInits;

	// All remaining function arguments that may be inaccessible in the function body, are moved
	// at the beginning of the function body.
	for (TypedName const& param: _functionDefinition.parameters)
		if (auto slot = m_memoryOffsetTracker(param.name))
			memoryVariableInits += generateMemoryStore(
				m_context.dialect,
				param.location,
				*slot,
				Identifier{param.location, param.name}
			);

	// All memory return variables have to be initialized to zero in memory.
	for (TypedName const& returnVariable: _functionDefinition.returnVariables)
		if (auto slot = m_memoryOffsetTracker(returnVariable.name))
			memoryVariableInits += generateMemoryStore(
				m_context.dialect,
				returnVariable.location,
				*slot,
				Literal{returnVariable.location, LiteralKind::Number, "0"_yulstring, {}}
			);

	if (!memoryVariableInits.empty())
	{
		memoryVariableInits += move(_functionDefinition.body.statements);
		_functionDefinition.body.statements = move(memoryVariableInits);
	}

	for (optional<YulString> slot: moveInfo.returnVariableSlots)
		if (slot)
			_functionDefinition.returnVariables.pop_back();
	for (TypedName const& returnVariable: _functionDefinition.returnVariables)
		if (auto slot = m_memoryOffsetTracker(returnVariable.name))
			_functionDefinition.body.statements.emplace_back(
				Assignment{
					returnVariable.location,
					{ Identifier{
						returnVariable.location,
						returnVariable.name
					} },
					make_unique<Expression>(
						generateMemoryLoad(
							m_context.dialect,
							returnVariable.location,
							*slot
						))
				}
			);
}

void StackToMemoryMover::operator()(Block& _block)
{
	using OptionalStatements = std::optional<vector<Statement>>;
	auto rewriteAssignmentOrVariableDeclarationToFunctionCall = [&](
		auto& _stmt,
		auto& _variables
	) -> OptionalStatements {
		using StatementType = decay_t<decltype(_stmt)>;
		if (_stmt.value)
			visit(*_stmt.value);
		if (!m_slotsForCurrentReturns)
			return {};

		langutil::SourceLocation  loc = _stmt.location;
		yulAssert(_variables.size() != 1, "");
		yulAssert(_stmt.value, "");

		std::vector<Statement> result;
		vector<VariableDeclaration> tempDecls;
		vector<Statement> memoryAssignments;
		vector<Statement> variableAssignments;
		for (optional<YulString> slot: *m_slotsForCurrentReturns | boost::adaptors::reversed)
			if (slot)
			{
				auto var = move(_variables.back());
				_variables.pop_back();
				variableAssignments.emplace_back(StatementType{
					loc, { std::move(var) },
					make_unique<Expression>(generateMemoryLoad(
						m_context.dialect,
						loc,
						*slot
					))
				});
			}
		if (_variables.empty())
			result.emplace_back(ExpressionStatement{loc, move(*_stmt.value)});
		else
			result.emplace_back(move(_stmt));

		m_slotsForCurrentReturns = nullptr;
		result += move(tempDecls);
		std::reverse(memoryAssignments.begin(), memoryAssignments.end());
		result += std::move(memoryAssignments);
		std::reverse(variableAssignments.begin(), variableAssignments.end());
		result += std::move(variableAssignments);
		return OptionalStatements{move(result)};
	};

	auto rewriteAssignmentOrVariableDeclarationLeftHandSide = [&](
		auto& _stmt,
		auto& _variables
	) -> OptionalStatements {
		using StatementType = decay_t<decltype(_stmt)>;
		if (!util::contains_if(_variables, [&](auto const& var)	{ return m_memoryOffsetTracker(var.name); }))
			return {};

		langutil::SourceLocation  loc = _stmt.location;
		if (_variables.size() == 1)
		{
			optional<YulString> offset = m_memoryOffsetTracker(_variables.front().name);
			yulAssert(offset, "");
			return generateMemoryStore(
				m_context.dialect,
				loc,
				*offset,
				_stmt.value ? *std::move(_stmt.value) : Literal{loc, LiteralKind::Number, "0"_yulstring, {}}
			);
		}
		yulAssert(_stmt.value, "");

		vector<Statement> memoryAssignments;
		vector<Statement> variableAssignments;
		VariableDeclaration tempDecl{loc, {}, std::move(_stmt.value)};
		for (auto& var: _variables)
		{
			YulString tempVarName = m_nameDispenser.newName(var.name);
			tempDecl.variables.emplace_back(TypedName{var.location, tempVarName, {}});

			if (optional<YulString> offset = m_memoryOffsetTracker(var.name))
				memoryAssignments += generateMemoryStore(
					m_context.dialect,
					loc,
					*offset,
					Identifier{loc, tempVarName}
				);
			else
				variableAssignments.emplace_back(StatementType{
					loc, { std::move(var) },
					make_unique<Expression>(Identifier{loc, tempVarName})
				});
		}

		std::vector<Statement> result;
		result.emplace_back(std::move(tempDecl));
		std::reverse(memoryAssignments.begin(), memoryAssignments.end());
		result += std::move(memoryAssignments);
		std::reverse(variableAssignments.begin(), variableAssignments.end());
		result += std::move(variableAssignments);
		return OptionalStatements{move(result)};
	};

	util::iterateReplacing(
		_block.statements,
		[&](Statement& _statement)
		{
			return std::visit(util::GenericVisitor{
				[&](Assignment& _assignment) -> OptionalStatements
				{
					return rewriteAssignmentOrVariableDeclarationToFunctionCall(_assignment, _assignment.variableNames);
				},
				[&](VariableDeclaration& _varDecl) -> OptionalStatements
				{
					return rewriteAssignmentOrVariableDeclarationToFunctionCall(_varDecl, _varDecl.variables);
				},
				[&](auto& _stmt) -> OptionalStatements { (*this)(_stmt); return {}; }
			}, _statement);
		}
	);
	util::iterateReplacing(
		_block.statements,
		[&](Statement& _statement)
		{
			return std::visit(util::GenericVisitor{
				[&](Assignment& _assignment) -> OptionalStatements
				{
					return rewriteAssignmentOrVariableDeclarationLeftHandSide(_assignment, _assignment.variableNames);
				},
				[&](VariableDeclaration& _varDecl) -> OptionalStatements
				{
					return rewriteAssignmentOrVariableDeclarationLeftHandSide(_varDecl, _varDecl.variables);
				},
				util::VisitorFallback<OptionalStatements>{}
			}, _statement);
		}
	);
}

void StackToMemoryMover::visit(Expression& _expression)
{
	ASTModifier::visit(_expression);
	if (Identifier* identifier = std::get_if<Identifier>(&_expression))
		if (optional<YulString> offset = m_memoryOffsetTracker(identifier->name))
			_expression = generateMemoryLoad(m_context.dialect, identifier->location, *offset);
}

optional<YulString> StackToMemoryMover::VariableMemoryOffsetTracker::operator()(YulString _variable) const
{
	if (m_memorySlots.count(_variable))
	{
		uint64_t slot = m_memorySlots.at(_variable);
		yulAssert(slot < m_numRequiredSlots, "");
		return YulString{util::toCompactHexWithPrefix(m_reservedMemory + 32 * (m_numRequiredSlots - slot - 1))};
	}
	else
		return std::nullopt;
}
