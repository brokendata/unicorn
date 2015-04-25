def mapper(sent: String): List[(String, Int)] =
  sent.split("\\s"). map ((_,1)) toList


def reducer(kv: List[(String, Int)]): Map[(String -> Int)]
