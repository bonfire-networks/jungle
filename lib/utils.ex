defmodule Jungle.Utils do
  @moduledoc """
  Utility functions used across the project.
  """

  @doc """
  Concatenates the string representations of the provided values.
  """
  def str(pieces) do
    pieces |> Enum.map(&to_string/1) |> Enum.join("")
  end

  @doc """
  Runs the given command and returns the output.
  """
  def run_command(command, args \\ [], opts \\ []) do
    case System.find_executable(to_string(command)) do
      nil ->
        raise "Command not found on PATH: #{command}"

      exe ->
        log_errors? = opts[:log_errors] != false

        {output, exit_code} =
          System.cmd(exe, Enum.map(args, &to_string/1), stderr_to_stdout: log_errors?)

        if exit_code == 0 do
          if output == "", do: :ok, else: {:ok, output}
        else
          if log_errors?, do: IO.warn("error running `#{command} #{inspect(args)}`: #{output}")

          {:error, output}
        end
    end
  end

  @doc """
  Runs the given function in parallel on the items in the collection, with a concurrency limit.
  """
  def concurrent_each(collection, fun, concurrency_limit \\ 50) do
    collection
    |> Task.async_stream(fun, max_concurrency: concurrency_limit)
    |> Enum.to_list()
  end

  @doc """
  Runs the given code block with stderr redirected to /dev/null.
  """
  def with_silent_stderr(block) do
    # {:ok, original} = :file.open(~c"/dev/null", [:write])

    try do
      #   :file.sync(original)
      #   :ok = :io.replace_stderr(original)
      block.()
    after
      #   :io.replace_stderr(:standard_error)
      #   :file.close(original)
    end
  end
end
