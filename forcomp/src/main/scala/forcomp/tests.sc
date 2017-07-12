object tests {

  //    println("aabcd".toList groupBy(c=>c).toString.map((c:Char, l:List[Char])=>(c,l.length)))
  val s = "aabcd".toList.groupBy((c: Char) => c).toList.map(t => (t._1, t._2.length))
  //
  //  val m = s.groupBy((c:Char)=>c)
  //  val l = m.toList
  //
  //  l.map(t=>(t._1,t._2.length))

  val dd = List('s', 'e', 'f').sorted


  val occ = List(('a', 1), ('d', 7), ('l', 1), ('r', 10))
  val elems = List(('d', 3), ('r', 10))
  val mapelems = elems.toMap


  val mapocc = occ.toMap

  mapelems.foldLeft(mapocc)((m, el) => {
    val elemInOcc = m apply el._1
    if (el._2 < elemInOcc) {
      m - el._1 updated (el._1, elemInOcc - el._2)
    } else {
      m - el._1
    }
  })


  //
  //    occ.toMap.foldLeft(('r', 4))((t1,t2)=> {
  //      println("t1 "+t1)
  //      println("t2 "+t2)
  //      if (t1._1==t2._1)
  //        (t1._1,t2._2 - t1._2)
  //      else t2
  //    })

}