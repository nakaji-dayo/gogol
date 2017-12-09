{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Drive.Teamdrives.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Team Drive\'s metadata by ID.
--
-- /See:/ <https://developers.google.com/drive/ Drive API Reference> for @drive.teamdrives.get@.
module Network.Google.Resource.Drive.Teamdrives.Get
    (
    -- * REST Resource
      TeamdrivesGetResource

    -- * Creating a Request
    , teamdrivesGet
    , TeamdrivesGet

    -- * Request Lenses
    , tgTeamDriveId
    , tgUseDomainAdminAccess
    , tgFields
    ) where

import Network.Google.Drive.Types
import Network.Google.Prelude

-- | A resource alias for @drive.teamdrives.get@ method which the
-- 'TeamdrivesGet' request conforms to.
type TeamdrivesGetResource =
     "drive" :>
       "v3" :>
         "teamdrives" :>
           Capture "teamDriveId" Text :>
             QueryParam "useDomainAdminAccess" Bool :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] TeamDrive

-- | Gets a Team Drive\'s metadata by ID.
--
-- /See:/ 'teamdrivesGet' smart constructor.
data TeamdrivesGet = TeamdrivesGet'
    { _tgTeamDriveId :: !Text
    , _tgUseDomainAdminAccess :: !Bool
    , _tgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TeamdrivesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgTeamDriveId'
--
-- * 'tgUseDomainAdminAccess'
--
-- * 'tgFields'
teamdrivesGet
    :: Text -- ^ 'tgTeamDriveId'
    -> TeamdrivesGet
teamdrivesGet pTgTeamDriveId_ = 
    TeamdrivesGet'
    { _tgTeamDriveId = pTgTeamDriveId_
    , _tgUseDomainAdminAccess = False
    , _tgFields = Nothing
    }

-- | The ID of the Team Drive
tgTeamDriveId :: Lens' TeamdrivesGet Text
tgTeamDriveId
  = lens _tgTeamDriveId
      (\ s a -> s{_tgTeamDriveId = a})

-- | Whether the request should be treated as if it was issued by a domain
-- administrator; if set to true, then the requester will be granted access
-- if they are an administrator of the domain to which the Team Drive
-- belongs.
tgUseDomainAdminAccess :: Lens' TeamdrivesGet Bool
tgUseDomainAdminAccess
  = lens _tgUseDomainAdminAccess
      (\ s a -> s{_tgUseDomainAdminAccess = a})

-- | Selector specifying which fields to include in a partial response.
tgFields :: Lens' TeamdrivesGet (Maybe Text)
tgFields = lens _tgFields (\ s a -> s{_tgFields = a})

instance GoogleRequest TeamdrivesGet where
        type Rs TeamdrivesGet = TeamDrive
        type Scopes TeamdrivesGet =
             '["https://www.googleapis.com/auth/drive",
               "https://www.googleapis.com/auth/drive.readonly"]
        requestClient TeamdrivesGet'{..}
          = go _tgTeamDriveId (Just _tgUseDomainAdminAccess)
              _tgFields
              (Just AltJSON)
              driveService
          where go
                  = buildClient (Proxy :: Proxy TeamdrivesGetResource)
                      mempty
