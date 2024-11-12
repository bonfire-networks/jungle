defmodule Jungle.GitRepoTest do
  use ExUnit.Case
  alias Jungle.GitRepo
  alias Jungle.Git
  #   alias Jungle.Utils

  setup do
    # Create a temporary Git repository for testing
    tmp_path = System.tmp_dir!()
    temp_dir = tmp_path <> "extensions/jungle_test_repo"
    File.rm_rf!(temp_dir)
    Git.run_command(["init", temp_dir])
    Git.run_command(["-C", temp_dir, "checkout", "-b", "develop"])
    Git.run_command(["-C", temp_dir, "commit", "--allow-empty", "-n", "-m", "Initial commit"])
    on_exit(fn -> File.rm_rf!(temp_dir) end)
    %{tmp_path: tmp_path, repo_path: temp_dir}
  end

  test "get_path/1 returns the absolute path of the repository", %{repo_path: repo_path} do
    repo = GitRepo.new(repo_path)
    assert GitRepo.get_path(repo) == repo_path
  end

  test "get_name/1 returns the name of the repository", %{repo_path: repo_path} do
    repo = GitRepo.new(repo_path)
    assert GitRepo.get_name(repo) == "jungle_test_repo"
  end

  test "is_git?/1 returns true for a valid Git repository", %{
    repo_path: repo_path,
    tmp_path: tmp_path
  } do
    repo = GitRepo.new(repo_path)
    assert GitRepo.is_git?(repo)
    refute Git.repo?(tmp_path)
  end

  test "get_branch/1 returns the current branch", %{repo_path: repo_path} do
    repo = GitRepo.new(repo_path)
    assert GitRepo.get_branch(repo) == "develop"
    assert repo.branch == "develop"
  end

  test "fetch/1 successfully fetches the remote branches", %{repo_path: repo_path} do
    repo = GitRepo.new(repo_path)
    assert GitRepo.fetch(repo) == :ok
  end

  test "list_repos/1 successfully lists all the repos", %{
    repo_path: repo_path,
    tmp_path: tmp_path
  } do
    assert Mix.Tasks.Jungle.list_repos(tmp_path) == [repo_path]
  end

  test "fetch_repos/1 successfully fetches all the remote branches", %{repo_path: repo_path} do
    assert Mix.Tasks.Jungle.fetch_repos([repo_path]) == [ok: :ok]
  end
end
