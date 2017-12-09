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
-- Module      : Network.Google.Resource.DFAReporting.UserProFiles.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves list of user profiles for a user.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.userProfiles.list@.
module Network.Google.Resource.DFAReporting.UserProFiles.List
    (
    -- * REST Resource
      UserProFilesListResource

    -- * Creating a Request
    , userProFilesList
    , UserProFilesList

    -- * Request Lenses
    , upflFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.userProfiles.list@ method which the
-- 'UserProFilesList' request conforms to.
type UserProFilesListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Get '[JSON] UserProFileList

-- | Retrieves list of user profiles for a user.
--
-- /See:/ 'userProFilesList' smart constructor.
newtype UserProFilesList = UserProFilesList'
    { _upflFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserProFilesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upflFields'
userProFilesList
    :: UserProFilesList
userProFilesList = 
    UserProFilesList'
    { _upflFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
upflFields :: Lens' UserProFilesList (Maybe Text)
upflFields
  = lens _upflFields (\ s a -> s{_upflFields = a})

instance GoogleRequest UserProFilesList where
        type Rs UserProFilesList = UserProFileList
        type Scopes UserProFilesList =
             '["https://www.googleapis.com/auth/dfareporting",
               "https://www.googleapis.com/auth/dfatrafficking"]
        requestClient UserProFilesList'{..}
          = go _upflFields (Just AltJSON) dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy UserProFilesListResource)
                      mempty
