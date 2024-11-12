defmodule Jungle.Git do
  @moduledoc """
  Functions for interacting with Git repositories.
  """

  alias Jungle.Utils

  def run_command(args, opts \\ []) do
    Utils.run_command("git", args, opts)
  end

  @doc """
  Fetches the remote branches for the given repository path.
  """
  def fetch(path) do
    run_command(["-C", path, "fetch", "--prune"])
  end

  @doc """
  Checks if the repository at the given path is a Git repository.
  """
  def repo?(path) do
    case run_command(["-C", path, "rev-parse"], log_errors: false) do
      {:error, _} -> false
      _ -> true
    end
  end

  @doc """
  Checks if the repository at the given path is currently in a rebase.
  """
  def in_rebase?(path) do
    case run_command(["-C", path, "rev-parse", "--git-path", "rebase-merge"]) do
      {:ok, output} -> File.dir?(Path.join(path, String.trim(output)))
      _ -> false
    end
  end

  @doc """
  Checks if the repository at the given path is currently in a merge.
  """
  def in_merge?(path) do
    case run_command(["-C", path, "rev-parse", "-q", "--verify", "MERGE_HEAD"]) do
      {:ok, _} -> true
      _ -> false
    end
  end

  def clone(repo, opts) do
    if branch = opts[:branch],
      do: run_command(["clone", "-b", branch, repo, opts[:path]]),
      else: run_command(["clone", repo, opts[:path]])
  end

  @doc """
  Returns the current branch of the repository at the given path.
  """
  def current_branch(path) do
    case run_command(["-C", path, "rev-parse", "--abbrev-ref", "HEAD"]) do
      {:ok, output} ->
        String.trim(output)

      _ ->
        nil
    end
  end

  @doc """
  Returns the HEAD commit of the repository at the given path.
  """
  def head(path) do
    Utils.with_silent_stderr(fn ->
      case run_command(["-C", path, "rev-parse", "--abbrev-ref", "@"]) do
        {:ok, output} -> String.trim(output)
        _ -> nil
      end
    end)
  end

  @doc """
  Returns the remote branch that the current branch is tracking, if any.
  """
  def tracking(path, branch \\ "") do
    Utils.with_silent_stderr(fn ->
      case run_command(["-C", path, "rev-parse", "--abbrev-ref", "#{branch}@{u}"]) do
        {:ok, output} -> String.trim(output)
        _ -> nil
      end
    end)
  end

  @doc """
  Returns the number of commits the current branch is behind the tracked remote branch.
  """
  def behind(path, from, to) do
    Utils.with_silent_stderr(fn ->
      case run_command(["-C", path, "rev-list", "#{from}..#{to}", "--count"]) do
        {:ok, output} -> String.to_integer(String.trim(output))
        _ -> nil
      end
    end)
  end

  @doc """
  Returns the number of commits the current branch is ahead of the tracked remote branch.
  """
  def ahead(path, from, to) do
    Utils.with_silent_stderr(fn ->
      case run_command(["-C", path, "rev-list", "#{from}..#{to}", "--count"]) do
        {:ok, output} -> String.to_integer(String.trim(output))
        _ -> nil
      end
    end)
  end

  #   @doc """
  #   Represents a change in a Git repository.
  #   """
  #   defmodule Change do
  #     @enforce_keys [:path, :staged, :unstaged, :untracked]
  #     defstruct [:path, :staged, :unstaged, :untracked]
  #   end

  #   @doc """
  #   Returns the changes in the repository at the given path.
  #   """
  #   def changes(path) do
  #     # Implement the logic to get the changes in the repository
  #     # This could involve parsing the output of `git status --porcelain`
  #     # and creating a list of `Jungle.Git.Change` structs
  #     []
  #   end
end
