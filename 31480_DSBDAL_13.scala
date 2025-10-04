object QuickSortApp {
  def quickSort(arr: List[Int]): List[Int] = {
    if (arr.length <= 1) arr
    else {
      val pivot = arr(arr.length / 2)
      quickSort(arr.filter(_ < pivot)) :::
      arr.filter(_ == pivot) :::
      quickSort(arr.filter(_ > pivot))
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = List(9, 4, 6, 2, 8, 1)
    println(s"Original List: $nums")
    val sorted = quickSort(nums)
    println(s"Sorted List: $sorted")
  }
}

object BubbleSortApp {
  def bubbleSort(arr: Array[Int]): Array[Int] = {
    val n = arr.length
    for (i <- 0 until n - 1) {
      for (j <- 0 until n - i - 1) {
        if (arr(j) > arr(j + 1)) {
          val temp = arr(j)
          arr(j) = arr(j + 1)
          arr(j + 1) = temp
        }
      }
    }
    arr
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(64, 34, 25, 12, 22, 11, 90)
    println(s"Original array: ${nums.mkString(", ")}")
    val sorted = bubbleSort(nums)
    println(s"Sorted array: ${sorted.mkString(", ")}")
  }
}


object WordCounter {
  def main(text: String): Unit = {
    val words = text.split("\\s+").toSeq
    val rdd = sc.parallelize(words)
    val counts = rdd.map(word => (word, 1)).reduceByKey(_ + _)
    counts.collect().foreach(println)
  }
}
