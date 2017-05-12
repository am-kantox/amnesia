#            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
#                    Version 2, December 2004
#
#            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
#   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
#
#  0. You just DO WHAT THE FUCK YOU WANT TO.

defmodule Amnesia.Helper do
  defmodule Binder do
    defmacro expanded_name!(value, mod \\ nil) do
        IO.inspect Macro.expand(value, __CALLER__), label: "EN"
      quote do
        IO.inspect Macro.expand(unquote(value), unquote(mod)), label: "EN"
        var!(expanded_name) = Macro.expand(unquote(value), unquote(mod))
      end
    end

    defmacro __using__(bindings) do
      [
        quote do
          bindings = unquote(bindings)
          defmacro __using__(_opts \\ []) do
            module = __MODULE__
            quote do
              require unquote(module), as: Bindings
              import unquote(module)
              Bindings.bindings!
              Bindings.attributes!
            end
          end
          defmacro bindings(), do: unquote(bindings)
          defmacro bindings!() do
            Enum.map(unquote(bindings), fn {attr, val} ->
              {:=, [], [{attr, [], nil}, val]}
            end)
          end
          defmacro attributes!() do
            Enum.map(unquote(bindings), fn {attr, val} ->
              quote do
                Module.register_attribute(__MODULE__, unquote(attr), accumulate: false)
                Module.put_attribute(__MODULE__, unquote(attr), unquote(val))
              end
            end)
          end
          defmacro sigil_b(key, _modifiers) do
            bindings = unquote(bindings)
            quote do: unquote(bindings)[String.to_atom(unquote(key))]
          end
        end |

        Enum.map(bindings, fn {attr, val} ->
          quote do
            defmacro unquote(attr)(), do: unquote(val)
            def unquote(:"table_#{attr}")(), do: unquote(val)
          end
        end)
      ]
    end
  end

  defmodule Options do
    @doc """
    Allows use of pipe operator to chain conditional updates
    to a keyword list. If the given value is nil, no update is
    done.

    Please note: mylist[:a][:b] will properly return nil even
    of mylist[:a] is already nil instead of crashing. This allows
    using even deeply nested values safely with this construct.
    """
    def update(args, key, value) do
      case value do
        nil  -> args
        _any -> Keyword.put(args, key, value)
      end
    end

    @doc "Turns parameter into a list, invariant when paramers is a list or nil."
    def normalize(data) when data |> is_list do
      data
    end

    def normalize(nil), do: nil

    def normalize(data) do
      [data]
    end
  end

  defmacro result(result) do
    quote do
      result = try do
        unquote(result)
      catch
        :exit, error ->
          error
      end

      case result do
        { :atomic, result } ->
          result

        { :aborted, { :amnesia, { :cancel, result } } } ->
          result

        { :aborted, { exception, stacktrace } } ->
          reraise Exception.normalize(:error, exception), stacktrace

        { :aborted, error } ->
          throw error

        result ->
          result
      end
    end
  end
end
