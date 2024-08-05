//> using dep "org.kohsuke:github-api:1.323"
//> using toolkit default
//> using scala 3

import org.kohsuke.github.GitHubBuilder
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import org.kohsuke.github.GHFileNotFoundException
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._
import org.kohsuke.github.GHRepository
import java.util.Date

val inactiveYears = 2
val inactiveDateCutoff = {
  val date = Date()
  date.setYear(date.getYear() - inactiveYears)
  date
}

case class RepoEntry(repo: String, branch: Option[String]):
  override def toString(): String = this match
    case RepoEntry(repo, Some(branch)) => s"$repo:$branch"
    case RepoEntry(repo, None)         => repo

enum RepoResult(repo: String, branch: Option[String], val isValid: Boolean):
  case Invalid(repo: String, branch: Option[String])
      extends RepoResult(repo, branch, isValid = false)
  case Archived(repo: String)
      extends RepoResult(repo, branch = None, isValid = false)
  case Correct(entry: RepoEntry)
      extends RepoResult(entry.repo, entry.branch, isValid = true)
  case Inactive(repo: String)
      extends RepoResult(repo, branch = None, isValid = false)

  override def toString(): String =
    this match
      case Invalid(repo, None)         => repo + " (doesn't exist)"
      case Invalid(repo, Some(branch)) => s"$repo:$branch (no such branch)"
      case Archived(value)             => s"$repo is archived"
      case Inactive(value) => s"$repo is inactive for last $inactiveYears"
      case Correct(_)      => s"$repo:$branch (OK)"

/** Potential improvements:
  *   - find active repositories with no opened Scala Steward PRs (potential
  *     issue)
  * Run with `scala-cli ./scripts/test-repos.scala -- $GITHUB_TOKEN `
  * @param githubToken
  */
@main
def main(
    githubToken: String
) =
  given ex: ExecutionContext = ExecutionContext.global

  val reposGihub = os.pwd / "repos-github.md"
  val repos = os
    .read(reposGihub)
    .split("\n")
    .toList
    .flatMap { repo =>
      repo.trim().stripPrefix("-").trim().split(":") match
        case Array(repo)         => Some(RepoEntry(repo, None))
        case Array(repo, branch) => Some(RepoEntry(repo, Some(branch)))
        case _                   => None

    }
  val gh = new GitHubBuilder()
    .withOAuthToken(githubToken)
    .build()

  def wrongBranch(ghRepo: GHRepository, repoEntry: RepoEntry) =
    repoEntry match
      case RepoEntry(repo, Some(branch)) =>
        Try(ghRepo.getBranch(branch)) match
          case Failure(exception) =>
            Some(RepoResult.Invalid(repoEntry.repo, Some(branch)))
          case Success(value) =>
            None
      case _ => None

  def archived(ghRepo: GHRepository, repoEntry: RepoEntry) =
    if ghRepo.isArchived() then Some(RepoResult.Archived(repoEntry.repo))
    else None

  def isInactive(ghRepo: GHRepository, repoEntry: RepoEntry) =
    val lastCommit = ghRepo.getBranch(ghRepo.getDefaultBranch()).getSHA1()
    val lastCommitDate = ghRepo.getCommit(lastCommit).getCommitDate()
    if lastCommitDate.before(inactiveDateCutoff) then
      Some(RepoResult.Inactive(repoEntry.repo))
    else None

  val nonExistent = repos
    .grouped(5)
    .flatMap: repos =>
      val futures = repos.map: repoEntry =>
        println(s"Analysing $repoEntry")
        Future:
          Try(gh.getRepository(repoEntry.repo)) match
            case Failure(exception: GHFileNotFoundException) =>
              RepoResult.Invalid(repoEntry.repo, repoEntry.branch)
            case Success(ghRepo) =>
              wrongBranch(ghRepo, repoEntry)
                .orElse(archived(ghRepo, repoEntry))
                .orElse(isInactive(ghRepo, repoEntry))
                .getOrElse(RepoResult.Correct(repoEntry))
            case _ => RepoResult.Correct(repoEntry)

      Await.result(Future.sequence(futures), 1.minute)
    .filter(!_.isValid)
    .toList

  if nonExistent.nonEmpty then
    println(s"\nIdentified problems with ${nonExistent.length} repositories:")
    nonExistent.foreach: repo =>
      println(s" - https://github.com/$repo")
