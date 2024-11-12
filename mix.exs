defmodule Jungle do
  use Mix.Project

  def project do
    [
      app: :jungle,
      version: "0.0.1",
      elixir: "~> 1.11",
      escript: [main_module: Mix.Tasks.Jungle]
    ]
  end
end
