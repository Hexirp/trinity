{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module Trinity where
  import Prelude

  import GHC.Generics

  import qualified Data.Set as Set

  import qualified Data.ByteString as Byte
  import qualified Data.ByteString.Lazy as Byte (fromStrict, toStrict)

  import qualified Data.Binary as Binary

  import qualified Data.Text as Text

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

  data Data = Data_ID !ID | Data_Text !Text.Text | Data_Int !Int
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving anyclass Binary.Binary

  newtype Object = Object { unwrap_object :: Byte.ByteString }
    deriving stock Eq
    deriving stock Ord
    deriving stock Show
    deriving stock Read
    deriving stock Generic
    deriving anyclass Binary.Binary

  from_data_to_object :: Data -> Object
  from_data_to_object = Object . Byte.toStrict . Binary.encode

  from_object_to_data :: Object -> Data
  from_object_to_data = Binary.decode . Byte.fromStrict . unwrap_object

  from_id_to_object :: ID -> Object
  from_id_to_object = from_data_to_object . Data_ID

  data Triple
    =
      Triple
        {
          triple_subject :: !ID,
          triple_predicate :: !ID,
          triple_object :: !Object,
          triple_id :: !ID,
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
    current_time <- get_current_time
    system_user_id <- generate_id
    adding_new_id_operation_id <- generate_id
    su_add_su_id_triple_id <- generate_id
    su_add_aio_id_triple_id <- generate_id

    pure
      (Model
        (Set.fromList
          [
            system_user_id,
            adding_new_id_operation_id,
            su_add_su_id_triple_id,
            su_add_aio_id_triple_id
          ])
        (Set.fromList
          [
            Triple
              system_user_id
              adding_new_id_operation_id
              (from_id_to_object system_user_id)
              su_add_su_id_triple_id
              current_time
              system_user_id,
            Triple
              system_user_id
              adding_new_id_operation_id
              (from_id_to_object adding_new_id_operation_id)
              su_add_aio_id_triple_id
              current_time
              system_user_id
          ]))
