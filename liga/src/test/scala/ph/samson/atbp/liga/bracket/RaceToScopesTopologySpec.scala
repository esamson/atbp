package ph.samson.atbp.liga.bracket

import zio.test.*

object RaceToScopesTopologySpec extends ZIOSpecDefault {

  def spec = suite("RaceToScopes topology alignment")(
    test("requiredKeys matches scopes from BracketTopology match ids") {
      val mismatches = (3 to 64).flatMap { playerCount =>
        val bracketSize = Seeding.bracketSize(playerCount)
        val fromTopology =
          BracketTopology(bracketSize).matches.keys
            .flatMap(RaceToScopes.keyForMatch)
            .toSet
        val fromFormula = RaceToScopes.requiredKeys(playerCount).toSet
        Option.when(fromTopology != fromFormula) {
          s"playerCount=$playerCount: topology=$fromTopology formula=$fromFormula"
        }
      }
      assertTrue(mismatches.isEmpty)
    }
  )
}
