import scala.io.Source
import scala.collection.mutable
import scala.collection.JavaConverters._
import java.util.TreeMap

/**
 * Usage: generate a git log file with each line as "committer @ message" format.
 *        e.g. "git log --pretty="%aN @ %s" |grep -i spark >spark.log"
 */
class GitStats {
  val EXCLUDE_LIST = Array("merge trunk", "merge from trunk", "merged from trunk")

  val DELEGATION_PATTERNS = Array("([\\w ]*) via".r, "\\(([\\w ]*) reviewed by".r)

  val mailMap = new mutable.HashMap[String,String]()

  def parse(logfile : String) = {
    val source = loadFile(logfile)
    val filterMerge = source.filter(line => EXCLUDE_LIST.filter(exclude => line.toLowerCase().contains(exclude.toLowerCase())).isEmpty)
    val splitted = filterMerge.map { line =>
      val arr = line.split("@")
      (arr(0).trim, arr(1).trim, arr(0).trim)
    }

    val checkins = new mutable.ArrayBuffer[(String, String, String)]()

    var toHandleCheckins = splitted
    var handledCheckins = ()
    DELEGATION_PATTERNS.foreach { pattern =>
      val (foundPatternList, notFoundPatternList) = toHandleCheckins.partition(pair => pattern.findFirstIn(pair._2).isDefined)
      toHandleCheckins = notFoundPatternList
      val newAuthorList = foundPatternList.map { pair =>
        pattern.findFirstIn(pair._2) match {
          case Some(pattern(author)) => (author.trim, pair._2, pair._1)
        }
      }
      checkins ++= newAuthorList
    }

    println("########################### Checkin by others: ")
    checkins.foreach(tuple => println(tuple._1+ ", "+tuple._2+ ", by "+tuple._3))
    println("---------------------------")
    println()

    val restCheckins = toHandleCheckins.toArray
    println("########################### Checkin by themselves: ")
    restCheckins.foreach(tuple => println(tuple._1+ ", "+tuple._2))
    println("---------------------------")
    println()

    checkins ++= restCheckins

    val  authorCount = checkins.foldLeft((new TreeMap[String, Int]()).asScala) { (map, checkin) =>
      map.update(checkin._1, map.getOrElse(checkin._1, 0)+1)
      map
    }

    authorCount.foreach(tuple => println(tuple._1+ ", "+tuple._2))

    val total = checkins.count(_ => true)
    println("total="+total)
  }

  private def loadFile(logfile: String) : Iterator[String] = {
    val source = Source.fromFile(logfile)
    val lineIter = source.getLines
    lineIter
  }
}

object GitStats extends App {

  val git = new GitStats
  if(args.length < 1) {
    println("Usage GitStats  log_file (in format: committer @ checkin)")
    sys.exit()
  }
  val file = args(0)
  git.parse(file)
}