package fileio

trait SimpleFileOps {
  def exists(path: String): Boolean
  def readFile(path: String): String
  def writeFile(path: String, data: String): Unit
}