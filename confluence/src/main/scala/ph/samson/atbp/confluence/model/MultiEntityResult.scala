package ph.samson.atbp.confluence.model

case class MultiEntityResult[T](results: List[T], _links: MultiEntityLinks)
