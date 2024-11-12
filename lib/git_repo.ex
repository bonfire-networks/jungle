defmodule Jungle.GitRepo do
  @moduledoc """
  Represents a Git repository.
  """

  alias Jungle.Git
  alias Jungle.Utils

  # , tracking: nil
  defstruct path: nil, branch: nil

  @doc """
  Creates a new Jungle.GitRepo struct for the given path.
  """
  def new(path) do
    %Jungle.GitRepo{
      path: path,
      branch: Git.current_branch(path)
      #   tracking: Git.tracking(path)
    }
  end

  @doc """
  Returns the absolute path of the repository.
  """
  def get_path(%Jungle.GitRepo{path: path}) do
    Path.expand(path)
  end

  @doc """
  Returns the name of the repository.
  """
  def get_name(%Jungle.GitRepo{path: path}) do
    Path.basename(path)
  end

  @doc """
  Fetches the remote branches for the repository.
  """
  def fetch(%Jungle.GitRepo{path: path}) do
    Git.fetch(path)
  end

  @doc """
  Returns the current branch of the repository.
  """
  def get_branch(%Jungle.GitRepo{path: path}) do
    Git.current_branch(path)
  end

  @doc """
  Returns the remote branch that the current branch is tracking, if any.
  """
  def get_tracking(%Jungle.GitRepo{path: path, branch: branch}) do
    Git.tracking(path, branch)
  end

  @doc """
  Checks if the repository is a Git repository.
  """
  def is_git?(%Jungle.GitRepo{path: path}) do
    Git.repo?(path)
  end

  @doc """
  Checks if the repository is currently in a rebase.
  """
  def in_rebase?(%Jungle.GitRepo{path: path}) do
    Git.in_rebase?(path)
  end

  @doc """
  Checks if the repository is currently in a merge.
  """
  def in_merge?(%Jungle.GitRepo{path: path}) do
    Git.in_merge?(path)
  end

  #   @doc """
  #   Returns the changes in the repository.
  #   """
  #   def get_changes(%Jungle.GitRepo{path: path}) do
  #     # Implement the logic to get the changes in the repository
  #     # This could involve parsing the output of `git status --porcelain`
  #     # and creating a list of `Jungle.Git.Change` structs
  #     []
  #   end

  #   @doc """
  #   Returns the number of staged changes in the repository.
  #   """
  #   def count_staged_changes(%Jungle.GitRepo{} = repo) do
  #     repo
  #     |> get_changes()
  #     |> Enum.count(& &1.staged)
  #   end

  #   @doc """
  #   Returns the number of unstaged changes in the repository.
  #   """
  #   def count_unstaged_changes(%Jungle.GitRepo{} = repo) do
  #     repo
  #     |> get_changes()
  #     |> Enum.count(& &1.unstaged)
  #   end

  #   @doc """
  #   Returns the number of untracked files in the repository.
  #   """
  #   def count_untracked(%Jungle.GitRepo{} = repo) do
  #     repo
  #     |> get_changes()
  #     |> Enum.count(& &1.untracked)
  #   end
end
