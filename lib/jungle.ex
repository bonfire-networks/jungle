defmodule Mix.Tasks.Jungle do
  alias Jungle.GitRepo
  alias Jungle.Git

  # for running as escript
  def main(args) do
    run(args)
  end

  # when running as Mix task
  def run(args) do
    # TODO: use args to specify which repos to fetch (either repo paths, or paths of regular directories in which to look for repos)
    list_repos()
    |> fetch_repos()
  end

  def fetch_repos(repos) do
    # Fetch each repository in parallel
    repos
    |> Jungle.Utils.concurrent_each(&fetch_repo/1)
  end

  defp fetch_repo(repo_path) do
    repo = GitRepo.new(repo_path)
    IO.puts("Fetching #{GitRepo.get_name(repo)}")
    Git.fetch(GitRepo.get_path(repo))
  end

  def list_repos(relative_to \\ File.cwd!()) do
    # find all the repositories here
    ([relative_to] ++
       Path.wildcard("#{relative_to}/extensions/*") ++ Path.wildcard("#{relative_to}/forks/*"))
    |> Enum.filter(&Git.repo?/1)

    # |> IO.inspect()
  end
end
