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
    defdatabase!(name, bind_quoted: [], do: block)
  end
  def defdatabase!(name, bind_quoted: bind_quoted, do: block) do
    binding_module_quoted =  with {:__aliases__, line, names} <- name do
                               {:__aliases__, line, names ++ [:Bindings]}
                             end
    quote do
      binding_module = unquote(binding_module_quoted)
      IO.inspect(binding_module, label: "★1")

      # unless [] == unquote(bind_quoted) do
        defmodule binding_module do
          require Amnesia.Helper.Producer, as: P
          P.macros_for_module(unquote(bind_quoted))
        end
      # end

      defmodule unquote(name) do
        use   Amnesia.Database
        alias Amnesia.Metadata

        IO.inspect(unquote(binding_module_quoted), label: "★2")
        # Code.eval_string("require #{binding_module}\n alias #{binding_module}")
        require unquote(binding_module_quoted), as: Bindings
        # IO.inspect "#{Bindings.user_table()}", label: "★☆★"

        bindings = unquote(bind_quoted)
        IO.inspect(Bindings.__info__(:macros), label: "★2")
        # alias #{Module.concat(unquote(name), "Bindings")}, as: Bindings
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
    expanded_name = case name do
      {t, meta, _} when is_tuple(t) ->
        atom = name
               |> Macro.expand(__CALLER__)
               |> Atom.to_string
               |> String.trim_leading("Elixir.")
               |> String.to_atom
        {:__aliases__, Keyword.merge([counter: 0], meta), [atom]}
      _ -> name
    end

    if attributes do
      [ Amnesia.Table.Definition.define(__CALLER__.module, expanded_name, attributes, Keyword.merge(opts, do_block)),

        # add the defined table to the list
        quote do: @tables unquote(expanded_name) ]
    else
      quote do
        alias __MODULE__.unquote(expanded_name)
      end
    end
  end
end
