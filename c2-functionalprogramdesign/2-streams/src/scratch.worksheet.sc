val testTerrain1: String =
        """ooo-------
          |oSoooo----
          |ooooooooo-
          |-ooooooooo
          |-----ooToo
          |------ooo-""".stripMargin

    print(testTerrain1.toList)
    testTerrain1.length

    for {
        lineArray <- testTerrain1.split("\n").toVector
    } yield lineArray.toVector
    testTerrain1.split("\n")