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
-- Module      : Network.Google.Resource.Analytics.Management.AccountUserLinks.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates permissions for an existing user on the given account.
--
-- /See:/ <https://developers.google.com/analytics/ Google Analytics API Reference> for @analytics.management.accountUserLinks.update@.
module Network.Google.Resource.Analytics.Management.AccountUserLinks.Update
    (
    -- * REST Resource
      ManagementAccountUserLinksUpdateResource

    -- * Creating a Request
    , managementAccountUserLinksUpdate
    , ManagementAccountUserLinksUpdate

    -- * Request Lenses
    , mauluPayload
    , mauluAccountId
    , mauluLinkId
    , mauluFields
    ) where

import Network.Google.Analytics.Types
import Network.Google.Prelude

-- | A resource alias for @analytics.management.accountUserLinks.update@ method which the
-- 'ManagementAccountUserLinksUpdate' request conforms to.
type ManagementAccountUserLinksUpdateResource =
     "analytics" :>
       "v3" :>
         "management" :>
           "accounts" :>
             Capture "accountId" Text :>
               "entityUserLinks" :>
                 Capture "linkId" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] EntityUserLink :>
                         Put '[JSON] EntityUserLink

-- | Updates permissions for an existing user on the given account.
--
-- /See:/ 'managementAccountUserLinksUpdate' smart constructor.
data ManagementAccountUserLinksUpdate = ManagementAccountUserLinksUpdate'
    { _mauluPayload :: !EntityUserLink
    , _mauluAccountId :: !Text
    , _mauluLinkId :: !Text
    , _mauluFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ManagementAccountUserLinksUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mauluPayload'
--
-- * 'mauluAccountId'
--
-- * 'mauluLinkId'
--
-- * 'mauluFields'
managementAccountUserLinksUpdate
    :: EntityUserLink -- ^ 'mauluPayload'
    -> Text -- ^ 'mauluAccountId'
    -> Text -- ^ 'mauluLinkId'
    -> ManagementAccountUserLinksUpdate
managementAccountUserLinksUpdate pMauluPayload_ pMauluAccountId_ pMauluLinkId_ = 
    ManagementAccountUserLinksUpdate'
    { _mauluPayload = pMauluPayload_
    , _mauluAccountId = pMauluAccountId_
    , _mauluLinkId = pMauluLinkId_
    , _mauluFields = Nothing
    }

-- | Multipart request metadata.
mauluPayload :: Lens' ManagementAccountUserLinksUpdate EntityUserLink
mauluPayload
  = lens _mauluPayload (\ s a -> s{_mauluPayload = a})

-- | Account ID to update the account-user link for.
mauluAccountId :: Lens' ManagementAccountUserLinksUpdate Text
mauluAccountId
  = lens _mauluAccountId
      (\ s a -> s{_mauluAccountId = a})

-- | Link ID to update the account-user link for.
mauluLinkId :: Lens' ManagementAccountUserLinksUpdate Text
mauluLinkId
  = lens _mauluLinkId (\ s a -> s{_mauluLinkId = a})

-- | Selector specifying which fields to include in a partial response.
mauluFields :: Lens' ManagementAccountUserLinksUpdate (Maybe Text)
mauluFields
  = lens _mauluFields (\ s a -> s{_mauluFields = a})

instance GoogleRequest
         ManagementAccountUserLinksUpdate where
        type Rs ManagementAccountUserLinksUpdate =
             EntityUserLink
        type Scopes ManagementAccountUserLinksUpdate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient ManagementAccountUserLinksUpdate'{..}
          = go _mauluAccountId _mauluLinkId _mauluFields
              (Just AltJSON)
              _mauluPayload
              analyticsService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ManagementAccountUserLinksUpdateResource)
                      mempty
