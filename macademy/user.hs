type UserId = Int
type SubscriberId = Maybe Int
type FBId = Maybe Int
type MSISDN = Maybe String
type Email = Maybe String
type DeviceId = String
type Devices = [DeviceId]

data User = User UserId SubscriberId FBId MSISDN Email Devices

data UserData = UserData User ...


getAuthenticationIds :: UserId -> (SubscriberId, FBId, MSISDN, Email, Devices)


findUserByFbId :: FBId -> Maybe User
findUserByMSISDN :: MSISDN -> Maybe User



userA = User 1 Nothing 16262 Nothing Nothing ["iPhone5A"]


login :: FBId -> DeviceId -> User
login fbid deviceId
	| exists deviceId fbid = User 1 Nothing 16262 Nothing Nothing ["iPhone5A"] -- no side effect
	| exists fbId = User 1 Nothing fbId Nothing Nothing ["iPhone5A", deviceId] -- add the device to the user
	| doesntExists fbId deviceId = User 2 Nothing fbId Nothing Nothing [deviceId]  -- add the user
	| doesntExists fbId and exists deviceId =
		let update users remove deviceId -- remove the device from the previous owner
		 User 2 Nothing fbId Nothing Nothing [deviceId] -- add the user

