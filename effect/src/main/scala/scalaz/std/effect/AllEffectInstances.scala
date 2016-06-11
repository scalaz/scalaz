package scalaz.std.effect

trait AllEffectInstances
    extends CloseableInstances
    with sql.ConnectionInstances
    with sql.StatementInstances
    with sql.PreparedStatementInstances
    with sql.ResultSetInstances

object AllEffectInstances extends AllEffectInstances
