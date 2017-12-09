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
-- Module      : Network.Google.Resource.Drive.Teamdrives.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Team Drive\'s metadata
--
-- /See:/ <https://developers.google.com/drive/ Drive API Reference> for @drive.teamdrives.update@.
module Network.Google.Resource.Drive.Teamdrives.Update
    (
    -- * REST Resource
      TeamdrivesUpdateResource

    -- * Creating a Request
    , teamdrivesUpdate
    , TeamdrivesUpdate

    -- * Request Lenses
    , tuPayload
    , tuTeamDriveId
    , tuFields
    ) where

import Network.Google.Drive.Types
import Network.Google.Prelude

-- | A resource alias for @drive.teamdrives.update@ method which the
-- 'TeamdrivesUpdate' request conforms to.
type TeamdrivesUpdateResource =
     "drive" :>
       "v3" :>
         "teamdrives" :>
           Capture "teamDriveId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] TeamDrive :> Patch '[JSON] TeamDrive

-- | Updates a Team Drive\'s metadata
--
-- /See:/ 'teamdrivesUpdate' smart constructor.
data TeamdrivesUpdate = TeamdrivesUpdate'
    { _tuPayload :: !TeamDrive
    , _tuTeamDriveId :: !Text
    , _tuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TeamdrivesUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuPayload'
--
-- * 'tuTeamDriveId'
--
-- * 'tuFields'
teamdrivesUpdate
    :: TeamDrive -- ^ 'tuPayload'
    -> Text -- ^ 'tuTeamDriveId'
    -> TeamdrivesUpdate
teamdrivesUpdate pTuPayload_ pTuTeamDriveId_ = 
    TeamdrivesUpdate'
    { _tuPayload = pTuPayload_
    , _tuTeamDriveId = pTuTeamDriveId_
    , _tuFields = Nothing
    }

-- | Multipart request metadata.
tuPayload :: Lens' TeamdrivesUpdate TeamDrive
tuPayload
  = lens _tuPayload (\ s a -> s{_tuPayload = a})

-- | The ID of the Team Drive
tuTeamDriveId :: Lens' TeamdrivesUpdate Text
tuTeamDriveId
  = lens _tuTeamDriveId
      (\ s a -> s{_tuTeamDriveId = a})

-- | Selector specifying which fields to include in a partial response.
tuFields :: Lens' TeamdrivesUpdate (Maybe Text)
tuFields = lens _tuFields (\ s a -> s{_tuFields = a})

instance GoogleRequest TeamdrivesUpdate where
        type Rs TeamdrivesUpdate = TeamDrive
        type Scopes TeamdrivesUpdate =
             '["https://www.googleapis.com/auth/drive"]
        requestClient TeamdrivesUpdate'{..}
          = go _tuTeamDriveId _tuFields (Just AltJSON)
              _tuPayload
              driveService
          where go
                  = buildClient
                      (Proxy :: Proxy TeamdrivesUpdateResource)
                      mempty
