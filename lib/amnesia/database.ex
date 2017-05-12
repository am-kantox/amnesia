#            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
#                    Version 2, December 2004
#
#            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
#   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
#
#  0. You just DO WHAT THE FUCK YOU WANT TO.

defmodule Amnesia.Database do
  @doc false
  defmacro __using__(_opts) do
    quote do
      import Amnesia.Database

      # this is needed to populate the tables present in the database
      # definition
      Module.register_attribute __MODULE__, :tables, accumulate: true
    end
  end

  @doc false
  def defdatabase!(name, do: block) do
    defdatabase!(name, bindings: [], do: block)
  end
  def defdatabase!(name, bindings: bindings, do: block) do
    binding_module = with {:__aliases__, line, names} <- name do
                       {:__aliases__, line, names ++ [:Bindings]}
                     end
    quote do

      defmodule unquote(binding_module) do
        use Amnesia.Helper.Binder, unquote(bindings)
      end

      defmodule unquote(name) do
        use   Amnesia.Database
        alias Amnesia.Metadata
        use unquote(binding_module)

        unquote(block)

        @doc """
        Alias all the table names in the current scope and require what's
        needed.
        """
        defmacro __using__(_opts) do
          [ quote(do: require Amnesia),
            quote(do: require Amnesia.Fragment),
            quote(do: require Exquisite),

            quote(do: alias unquote(__MODULE__)),
            Enum.map(@tables, fn module ->
              to = Module.split(module)
                |> Enum.drop(Module.split(__MODULE__) |> length)

              to = if length(to) > 1 do
                Module.concat(__MODULE__, to |> hd)
              else
                module
              end

              [ quote(do: alias unquote(to)),
                quote(do: require unquote(module)) ]
            end) ] |> List.flatten
        end

        @doc """
        List of atoms of the defined tables in the database.
        """
        @spec tables :: [atom]
        def tables do
          @tables
        end

        @doc """
        Create the database, it calls `.create` on every defined table.
        """
        @spec create :: [Amnesia.Table.o]
        @spec create(Amnesia.Table.c) :: [Amnesia.Table.o]
        def create(copying \\ []) do
          [ metadata() |> Metadata.create(copying: copying) |

            Enum.map(@tables, fn(table) ->
              table.create(copying)
            end) ]
        end

        @doc """
        Create the database, it calls `.create` on every defined table, raises
        in case of error.
        """
        @spec create! :: [Amnesia.Table.o]
        @spec create!(Amnesia.Table.c) :: [Amnesia.Table.o]
        def create!(copying \\ []) do
          metadata() |> Metadata.create!(copying: copying)

          Enum.each @tables, fn(table) ->
            table.create!(copying)
          end
        end

        @doc """
        Destroy the database, it calls `.destroy` on every defined table.
        """
        @spec destroy :: [Amnesia.Table.o]
        def destroy do
          [ metadata() |> Metadata.destroy |

            Enum.map(@tables, fn(table) ->
              table.destroy
            end) ]
        end

        @doc """
        Destroy the database, it calls `.destroy` on every defined table,
        raises in case of error.
        """
        @spec destroy! :: [Amnesia.Table.o]
        def destroy! do
          metadata() |> Metadata.destroy!

          Enum.each @tables, fn(table) ->
            table.destroy!
          end
        end

        @spec metadata :: Metadata.t
        def metadata do
          Metadata.for(__MODULE__)
        end

        @doc """
        Wait for the database to be loaded.
        """
        @spec wait :: :ok | { :timeout, [atom] } | { :error, atom }
        @spec wait(integer | :infinity) :: :ok | { :timeout, [atom] } | { :error, atom }
        def wait(timeout \\ :infinity) do
          Amnesia.Table.wait(@tables, timeout)
        end
      end
    end
  end

  defmacro expand(term) do
    Macro.expand(term, __CALLER__)
  end

  @doc """
  Define a table in the database with the given name, attributes and options.

  If only a name is given, it will forward declare a table.

  The created table will actually be a record, so you can define functions on
  it like you would normally do for a record, various helper methods are added
  by default.

  ## Options

  * `:indices` specifies a list of additional indices to use instead of the
    first attribute.
  * `:type` specifies the type of the table, it can be `:set`, `:ordered_set`
     and `:bag`, the default is `:set`
  * `:mode` specifies the access mode, it can be `:both` and `:read!`, the
    default is `:both`
  * `:majority` specifies the majority of the table, the default is `false`
  * `:priority` specifies the load priority of the table
  * `:local` specifies if the table is local, default is `false`

  ## Example

      use Amnesia

      defdatabase Foo do
        deftable Bar, [:id, :a], type: :bag

        deftable Baz, [:id, :a, :b] do
          def foo(self)
            42
          end
        end
      end

  """
  @spec deftable(atom) :: none
  @spec deftable(atom, [atom | { atom, any }]) :: none
  @spec deftable(atom, [atom | { atom, any }], Keyword.t) :: none
  @spec deftable(atom, [atom | { atom, any }], Keyword.t, Keyword.t) :: none
  defmacro deftable(name, attributes \\ nil, opts \\ [], do_block \\ []) do
    # IO.inspect __CALLER__.module, label: "★★★"
            IO.inspect name, label: "★★★"

    expanded_name = case name do
      {:aaa, b, var} ->
        # IO.inspect Module.get_attribute(__CALLER__.module, var), label: "★★★"
        # IO.inspect (Macro.var(name, nil)), label: "★★★"
        atom = __CALLER__.module
               |> Module.get_attribute(var)
               |> Module.concat(Bindings)
               |> apply(String.to_atom("table_#{name}"), [])
               |> Atom.to_string
               |> String.trim_leading("Elixir.")
               |> String.to_atom
        {:__aliases__, [counter: 0, line: __CALLER__.line], [atom]}
      {:sigil_b, meta, [{:<<>>, _, [item]}, []]} ->
        atom = __CALLER__.module
               |> Module.concat(Bindings)
               |> apply(String.to_atom("table_#{item}"), [])
               |> Atom.to_string
               |> String.trim_leading("Elixir.")
               |> String.to_atom
        {:__aliases__, Keyword.merge([counter: 0], meta), [atom]}
      {t, meta, _} when is_tuple(t) ->
        atom = name
               |> Macro.expand(__CALLER__)
               |> Atom.to_string
               |> String.trim_leading("Elixir.")
               |> String.to_atom
        {:__aliases__, Keyword.merge([counter: 0], meta), [atom]}
      _ ->
        name
    end

        IO.puts("1: #{inspect expanded_name}")
        if __CALLER__.module == DiscOnly.Database do
          require Amnesia.Helper.Binder
          Amnesia.Helper.Binder.expanded_name! expanded_name, __CALLER__
        end
        IO.puts("2: #{inspect expanded_name}")

    if attributes do
      [
        Amnesia.Table.Definition.define(__CALLER__.module, expanded_name, attributes, Keyword.merge(opts, do_block)),

        # add the defined table to the list
        quote do: @tables unquote(expanded_name) ]
    else
      quote do
        alias __MODULE__.unquote(expanded_name)
      end
    end
  end

#   defmacro deftables(name_or_list, attributes \\ nil, opts \\ [], do_block \\ [])

  defmacro deftables(list) when is_list(list) do
    Enum.each(list, fn
      opts when is_list(opts) ->
        name = Keyword.get(opts, :name)
        attributes = Keyword.get(opts, :attributes)
        opts = Keyword.get(opts, :opts, [])
        do_block = Keyword.get(opts, :do_block, [])
        quote do: deftable unquote(name), unquote(attributes), unquote(opts), unquote(do_block)
      opts when is_atom(opts) -> # atom, defaults
        quote do: deftable unquote(opts)
      end)
  end

  defmacro deftables(name, attributes, opts, do_block) do
    IO.inspect [name: name, attributes: attributes, opts: opts, do_block: do_block], "★★★"
    deftables([[name: name, attributes: attributes, opts: opts, do_block: do_block]])
  end

end
