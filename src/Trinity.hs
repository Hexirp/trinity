{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module Trinity where
  import Prelude

  import GHC.Generics

  import qualified Data.Set as Set

  import qualified Data.ByteString as Byte

  import qualified Data.Binary as Binary

  import qualified Data.Time as Time

  import qualified Data.UUID as UUID
  import qualified Data.UUID.V4 as UUID

  newtype Derived_By_Show_And_Read a
    = Derived_By_Show_And_Read { unwrap_Derived_By_Show_And_Read :: a }

  instance (Show a, Read a) => Binary.Binary (Derived_By_Show_And_Read a) where
    put = Binary.put . show . unwrap_Derived_By_Show_And_Read
    get = Derived_By_Show_And_Read . read <$> Binary.get

  newtype ID = ID UUID.UUID
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving anyclass Binary.Binary

  generate_id :: IO ID
  generate_id = ID <$> UUID.nextRandom

  newtype Time = Time Time.UTCTime
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving Binary.Binary via Derived_By_Show_And_Read Time

  get_current_time :: IO Time
  get_current_time = Time <$> Time.getCurrentTime

  newtype Object = Object { unwrap_object :: Byte.ByteString }
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving anyclass Binary.Binary

  data Triple
    =
      Triple
        {
          triple_id :: !ID,
          triple_subject :: !ID,
          triple_predicate :: !ID,
          triple_object :: !Object,
          triple_time :: !Time,
          triple_author :: !ID
        }
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving anyclass Binary.Binary

  data Model
    =
      Model
        {
          model_id_set :: !(Set.Set ID),
          model_triple_set :: !(Set.Set Triple)
        }
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving anyclass Binary.Binary

  generate_initial_model :: IO Model
  generate_initial_model = do
    system_user_id <- generate_id
    adding_operation_id <- generate_id
    return undefined
