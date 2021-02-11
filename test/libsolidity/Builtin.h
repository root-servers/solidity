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

#pragma once

#include <functional>
#include <map>
#include <optional>
#include <test/libsolidity/util/SoltestErrors.h>
#include <test/libsolidity/util/SoltestTypes.h>
#include <utility>
#include <vector>
#include <memory>

namespace solidity::frontend::test
{

class TestFunctionCall;

class Builtin
{
	/// This is the actual implementation of the builtin function.
	std::function<std::optional<bytes>(FunctionCall const&)> m_builtin = nullptr;

public:
	Builtin() = default;
	virtual ~Builtin() = default;

	explicit Builtin(std::function<std::optional<bytes>(FunctionCall const&)> _builtin)
	{
		m_builtin = std::move(_builtin);
	}

	virtual std::optional<bytes> builtin(FunctionCall const& _call)
	{
		if (m_builtin != nullptr)
			return m_builtin(_call);
		return std::nullopt;
	}
};

using Builtins = std::map<std::string, std::map<std::string, std::shared_ptr<Builtin>>>;

} // namespace solidity::frontend::test
