defmodule GMO.PG.MixProject do
  use Mix.Project

  def project() do
    [
      app: :gmo_pg,
      version: "0.0.0",
      elixir: "~> 1.7",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      name: "gmo_pg",
      package: package(),
      source_url: "https://github.com/potatosalad/erlang-gmo_pg"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application() do
    [
      extra_applications: [],
      mod: {:gmo_pg_app, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:hackney, ">= 0.0.0"},
      {:iconv, ">= 0.0.0"}
    ]
  end

  defp description() do
    """
    GMO-PG Payment Gateway Client for Erlang and Elixir
    """
  end

  defp package() do
    [
      name: :gmo_pg,
      files: [
        "CHANGELOG*",
        "LICENSE*",
        "mix.exs",
        "README*",
        "rebar.config",
        "src"
      ],
      licenses: ["Mozilla Public License Version 2.0"],
      links: %{
        "GitHub" => "https://github.com/potatosalad/erlang-gmo_pg"
      },
      maintainers: ["Andrew Bennett"]
    ]
  end
end
