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
-- Module      : Network.Google.Resource.People.People.GetBatchGet
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a list of specific people by specifying a
-- list of requested resource names. Use \`people\/me\` to indicate the
-- authenticated user.
-- The request throws a 400 error if \'personFields\' is not specified.
--
-- /See:/ <https://developers.google.com/people/ Google People API Reference> for @people.people.getBatchGet@.
module Network.Google.Resource.People.People.GetBatchGet
    (
    -- * REST Resource
      PeopleGetBatchGetResource

    -- * Creating a Request
    , peopleGetBatchGet
    , PeopleGetBatchGet

    -- * Request Lenses
    , pgbgXgafv
    , pgbgUploadProtocol
    , pgbgRequestMaskIncludeField
    , pgbgPp
    , pgbgAccessToken
    , pgbgUploadType
    , pgbgPersonFields
    , pgbgBearerToken
    , pgbgResourceNames
    , pgbgFields
    , pgbgCallback
    ) where

import Network.Google.People.Types
import Network.Google.Prelude

-- | A resource alias for @people.people.getBatchGet@ method which the
-- 'PeopleGetBatchGet' request conforms to.
type PeopleGetBatchGetResource =
     "v1" :>
       "people:batchGet" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "requestMask.includeField" FieldMask :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "personFields" FieldMask :>
                       QueryParam "bearer_token" Text :>
                         QueryParams "resourceNames" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] GetPeopleResponse

-- | Provides information about a list of specific people by specifying a
-- list of requested resource names. Use \`people\/me\` to indicate the
-- authenticated user.
-- The request throws a 400 error if \'personFields\' is not specified.
--
-- /See:/ 'peopleGetBatchGet' smart constructor.
data PeopleGetBatchGet = PeopleGetBatchGet'
    { _pgbgXgafv :: !(Maybe Xgafv)
    , _pgbgUploadProtocol :: !(Maybe Text)
    , _pgbgRequestMaskIncludeField :: !(Maybe FieldMask)
    , _pgbgPp :: !Bool
    , _pgbgAccessToken :: !(Maybe Text)
    , _pgbgUploadType :: !(Maybe Text)
    , _pgbgPersonFields :: !(Maybe FieldMask)
    , _pgbgBearerToken :: !(Maybe Text)
    , _pgbgResourceNames :: !(Maybe [Text])
    , _pgbgFields :: !(Maybe Text)
    , _pgbgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PeopleGetBatchGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgbgXgafv'
--
-- * 'pgbgUploadProtocol'
--
-- * 'pgbgRequestMaskIncludeField'
--
-- * 'pgbgPp'
--
-- * 'pgbgAccessToken'
--
-- * 'pgbgUploadType'
--
-- * 'pgbgPersonFields'
--
-- * 'pgbgBearerToken'
--
-- * 'pgbgResourceNames'
--
-- * 'pgbgFields'
--
-- * 'pgbgCallback'
peopleGetBatchGet
    :: PeopleGetBatchGet
peopleGetBatchGet = 
    PeopleGetBatchGet'
    { _pgbgXgafv = Nothing
    , _pgbgUploadProtocol = Nothing
    , _pgbgRequestMaskIncludeField = Nothing
    , _pgbgPp = True
    , _pgbgAccessToken = Nothing
    , _pgbgUploadType = Nothing
    , _pgbgPersonFields = Nothing
    , _pgbgBearerToken = Nothing
    , _pgbgResourceNames = Nothing
    , _pgbgFields = Nothing
    , _pgbgCallback = Nothing
    }

-- | V1 error format.
pgbgXgafv :: Lens' PeopleGetBatchGet (Maybe Xgafv)
pgbgXgafv
  = lens _pgbgXgafv (\ s a -> s{_pgbgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgbgUploadProtocol :: Lens' PeopleGetBatchGet (Maybe Text)
pgbgUploadProtocol
  = lens _pgbgUploadProtocol
      (\ s a -> s{_pgbgUploadProtocol = a})

-- | **Required.** Comma-separated list of person fields to be included in
-- the response. Each path should start with \`person.\`: for example,
-- \`person.names\` or \`person.photos\`.
pgbgRequestMaskIncludeField :: Lens' PeopleGetBatchGet (Maybe FieldMask)
pgbgRequestMaskIncludeField
  = lens _pgbgRequestMaskIncludeField
      (\ s a -> s{_pgbgRequestMaskIncludeField = a})

-- | Pretty-print response.
pgbgPp :: Lens' PeopleGetBatchGet Bool
pgbgPp = lens _pgbgPp (\ s a -> s{_pgbgPp = a})

-- | OAuth access token.
pgbgAccessToken :: Lens' PeopleGetBatchGet (Maybe Text)
pgbgAccessToken
  = lens _pgbgAccessToken
      (\ s a -> s{_pgbgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgbgUploadType :: Lens' PeopleGetBatchGet (Maybe Text)
pgbgUploadType
  = lens _pgbgUploadType
      (\ s a -> s{_pgbgUploadType = a})

-- | **Required.** A field mask to restrict which fields on each person are
-- returned. Valid values are: * addresses * ageRanges * biographies *
-- birthdays * braggingRights * coverPhotos * emailAddresses * events *
-- genders * imClients * locales * memberships * metadata * names *
-- nicknames * occupations * organizations * phoneNumbers * photos *
-- relations * relationshipInterests * relationshipStatuses * residences *
-- skills * taglines * urls
pgbgPersonFields :: Lens' PeopleGetBatchGet (Maybe FieldMask)
pgbgPersonFields
  = lens _pgbgPersonFields
      (\ s a -> s{_pgbgPersonFields = a})

-- | OAuth bearer token.
pgbgBearerToken :: Lens' PeopleGetBatchGet (Maybe Text)
pgbgBearerToken
  = lens _pgbgBearerToken
      (\ s a -> s{_pgbgBearerToken = a})

-- | The resource names of the people to provide information about. - To get
-- information about the authenticated user, specify \`people\/me\`. - To
-- get information about a google account, specify \`people\/\`account_id.
-- - To get information about a contact, specify the resource name that
-- identifies the contact as returned by
-- [\`people.connections.list\`](\/people\/api\/rest\/v1\/people.connections\/list).
-- You can include up to 50 resource names in one request.
pgbgResourceNames :: Lens' PeopleGetBatchGet [Text]
pgbgResourceNames
  = lens _pgbgResourceNames
      (\ s a -> s{_pgbgResourceNames = a})
      . _Default
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
pgbgFields :: Lens' PeopleGetBatchGet (Maybe Text)
pgbgFields
  = lens _pgbgFields (\ s a -> s{_pgbgFields = a})

-- | JSONP
pgbgCallback :: Lens' PeopleGetBatchGet (Maybe Text)
pgbgCallback
  = lens _pgbgCallback (\ s a -> s{_pgbgCallback = a})

instance GoogleRequest PeopleGetBatchGet where
        type Rs PeopleGetBatchGet = GetPeopleResponse
        type Scopes PeopleGetBatchGet =
             '["https://www.googleapis.com/auth/contacts",
               "https://www.googleapis.com/auth/contacts.readonly",
               "https://www.googleapis.com/auth/plus.login",
               "https://www.googleapis.com/auth/user.addresses.read",
               "https://www.googleapis.com/auth/user.birthday.read",
               "https://www.googleapis.com/auth/user.emails.read",
               "https://www.googleapis.com/auth/user.phonenumbers.read",
               "https://www.googleapis.com/auth/userinfo.email",
               "https://www.googleapis.com/auth/userinfo.profile"]
        requestClient PeopleGetBatchGet'{..}
          = go _pgbgXgafv _pgbgUploadProtocol
              _pgbgRequestMaskIncludeField
              (Just _pgbgPp)
              _pgbgAccessToken
              _pgbgUploadType
              _pgbgPersonFields
              _pgbgBearerToken
              (_pgbgResourceNames ^. _Default)
              _pgbgCallback
              _pgbgFields
              (Just AltJSON)
              peopleService
          where go
                  = buildClient
                      (Proxy :: Proxy PeopleGetBatchGetResource)
                      mempty
