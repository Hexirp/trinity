module Trinity where
  import Prelude

  import qualified Data.Set as Set

  import qualified Data.Time as Time

  import qualified Data.UUID.V4 as UUID

  newtype ID = ID UUID.UUID

  newtype Time = Time Time.UTCTime

  data Triple
    =
      Triple
        {
          triple_id :: ID,
          triple_subject :: ID,
          triple_predicate :: ID,
          triple_object :: ID,
          triple_time :: Time,
          triple_author :: ID
        }

  data Model
    =
      Model { model_id_set :: Set.Set ID, model_triple_set :: Set.Set Triple }
