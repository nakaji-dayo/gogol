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
-- Module      : Network.Google.Resource.People.People.Connections.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the authenticated user\'s contacts merged with any
-- connected profiles.
-- The request throws a 400 error if \'personFields\' is not specified.
--
-- /See:/ <https://developers.google.com/people/ Google People API Reference> for @people.people.connections.list@.
module Network.Google.Resource.People.People.Connections.List
    (
    -- * REST Resource
      PeopleConnectionsListResource

    -- * Creating a Request
    , peopleConnectionsList
    , PeopleConnectionsList

    -- * Request Lenses
    , pclSyncToken
    , pclXgafv
    , pclUploadProtocol
    , pclResourceName
    , pclRequestMaskIncludeField
    , pclRequestSyncToken
    , pclPp
    , pclAccessToken
    , pclUploadType
    , pclSortOrder
    , pclPersonFields
    , pclBearerToken
    , pclPageToken
    , pclPageSize
    , pclFields
    , pclCallback
    ) where

import Network.Google.People.Types
import Network.Google.Prelude

-- | A resource alias for @people.people.connections.list@ method which the
-- 'PeopleConnectionsList' request conforms to.
type PeopleConnectionsListResource =
     "v1" :>
       Capture "resourceName" Text :>
         "connections" :>
           QueryParam "syncToken" Text :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "requestMask.includeField" FieldMask :>
                   QueryParam "requestSyncToken" Bool :>
                     QueryParam "pp" Bool :>
                       QueryParam "access_token" Text :>
                         QueryParam "uploadType" Text :>
                           QueryParam "sortOrder" Text :>
                             QueryParam "personFields" FieldMask :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "pageToken" Text :>
                                   QueryParam "pageSize" (Textual Int32) :>
                                     QueryParam "callback" Text :>
                                       QueryParam "fields" Text :>
                                         QueryParam "alt" AltJSON :>
                                           Get '[JSON] ListConnectionsResponse

-- | Provides a list of the authenticated user\'s contacts merged with any
-- connected profiles.
-- The request throws a 400 error if \'personFields\' is not specified.
--
-- /See:/ 'peopleConnectionsList' smart constructor.
data PeopleConnectionsList = PeopleConnectionsList'
    { _pclSyncToken :: !(Maybe Text)
    , _pclXgafv :: !(Maybe Xgafv)
    , _pclUploadProtocol :: !(Maybe Text)
    , _pclResourceName :: !Text
    , _pclRequestMaskIncludeField :: !(Maybe FieldMask)
    , _pclRequestSyncToken :: !(Maybe Bool)
    , _pclPp :: !Bool
    , _pclAccessToken :: !(Maybe Text)
    , _pclUploadType :: !(Maybe Text)
    , _pclSortOrder :: !(Maybe Text)
    , _pclPersonFields :: !(Maybe FieldMask)
    , _pclBearerToken :: !(Maybe Text)
    , _pclPageToken :: !(Maybe Text)
    , _pclPageSize :: !(Maybe (Textual Int32))
    , _pclFields :: !(Maybe Text)
    , _pclCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PeopleConnectionsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pclSyncToken'
--
-- * 'pclXgafv'
--
-- * 'pclUploadProtocol'
--
-- * 'pclResourceName'
--
-- * 'pclRequestMaskIncludeField'
--
-- * 'pclRequestSyncToken'
--
-- * 'pclPp'
--
-- * 'pclAccessToken'
--
-- * 'pclUploadType'
--
-- * 'pclSortOrder'
--
-- * 'pclPersonFields'
--
-- * 'pclBearerToken'
--
-- * 'pclPageToken'
--
-- * 'pclPageSize'
--
-- * 'pclFields'
--
-- * 'pclCallback'
peopleConnectionsList
    :: Text -- ^ 'pclResourceName'
    -> PeopleConnectionsList
peopleConnectionsList pPclResourceName_ = 
    PeopleConnectionsList'
    { _pclSyncToken = Nothing
    , _pclXgafv = Nothing
    , _pclUploadProtocol = Nothing
    , _pclResourceName = pPclResourceName_
    , _pclRequestMaskIncludeField = Nothing
    , _pclRequestSyncToken = Nothing
    , _pclPp = True
    , _pclAccessToken = Nothing
    , _pclUploadType = Nothing
    , _pclSortOrder = Nothing
    , _pclPersonFields = Nothing
    , _pclBearerToken = Nothing
    , _pclPageToken = Nothing
    , _pclPageSize = Nothing
    , _pclFields = Nothing
    , _pclCallback = Nothing
    }

-- | A sync token, returned by a previous call to
-- \`people.connections.list\`. Only resources changed since the sync token
-- was created will be returned.
pclSyncToken :: Lens' PeopleConnectionsList (Maybe Text)
pclSyncToken
  = lens _pclSyncToken (\ s a -> s{_pclSyncToken = a})

-- | V1 error format.
pclXgafv :: Lens' PeopleConnectionsList (Maybe Xgafv)
pclXgafv = lens _pclXgafv (\ s a -> s{_pclXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pclUploadProtocol :: Lens' PeopleConnectionsList (Maybe Text)
pclUploadProtocol
  = lens _pclUploadProtocol
      (\ s a -> s{_pclUploadProtocol = a})

-- | The resource name to return connections for. Only \`people\/me\` is
-- valid.
pclResourceName :: Lens' PeopleConnectionsList Text
pclResourceName
  = lens _pclResourceName
      (\ s a -> s{_pclResourceName = a})

-- | **Required.** Comma-separated list of person fields to be included in
-- the response. Each path should start with \`person.\`: for example,
-- \`person.names\` or \`person.photos\`.
pclRequestMaskIncludeField :: Lens' PeopleConnectionsList (Maybe FieldMask)
pclRequestMaskIncludeField
  = lens _pclRequestMaskIncludeField
      (\ s a -> s{_pclRequestMaskIncludeField = a})

-- | Whether the response should include a sync token, which can be used to
-- get all changes since the last request.
pclRequestSyncToken :: Lens' PeopleConnectionsList (Maybe Bool)
pclRequestSyncToken
  = lens _pclRequestSyncToken
      (\ s a -> s{_pclRequestSyncToken = a})

-- | Pretty-print response.
pclPp :: Lens' PeopleConnectionsList Bool
pclPp = lens _pclPp (\ s a -> s{_pclPp = a})

-- | OAuth access token.
pclAccessToken :: Lens' PeopleConnectionsList (Maybe Text)
pclAccessToken
  = lens _pclAccessToken
      (\ s a -> s{_pclAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pclUploadType :: Lens' PeopleConnectionsList (Maybe Text)
pclUploadType
  = lens _pclUploadType
      (\ s a -> s{_pclUploadType = a})

-- | The order in which the connections should be sorted. Defaults to
-- \`LAST_MODIFIED_ASCENDING\`.
pclSortOrder :: Lens' PeopleConnectionsList (Maybe Text)
pclSortOrder
  = lens _pclSortOrder (\ s a -> s{_pclSortOrder = a})

-- | **Required.** A field mask to restrict which fields on each person are
-- returned. Valid values are: * addresses * ageRanges * biographies *
-- birthdays * braggingRights * coverPhotos * emailAddresses * events *
-- genders * imClients * locales * memberships * metadata * names *
-- nicknames * occupations * organizations * phoneNumbers * photos *
-- relations * relationshipInterests * relationshipStatuses * residences *
-- skills * taglines * urls
pclPersonFields :: Lens' PeopleConnectionsList (Maybe FieldMask)
pclPersonFields
  = lens _pclPersonFields
      (\ s a -> s{_pclPersonFields = a})

-- | OAuth bearer token.
pclBearerToken :: Lens' PeopleConnectionsList (Maybe Text)
pclBearerToken
  = lens _pclBearerToken
      (\ s a -> s{_pclBearerToken = a})

-- | The token of the page to be returned.
pclPageToken :: Lens' PeopleConnectionsList (Maybe Text)
pclPageToken
  = lens _pclPageToken (\ s a -> s{_pclPageToken = a})

-- | The number of connections to include in the response. Valid values are
-- between 1 and 2000, inclusive. Defaults to 100.
pclPageSize :: Lens' PeopleConnectionsList (Maybe Int32)
pclPageSize
  = lens _pclPageSize (\ s a -> s{_pclPageSize = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
pclFields :: Lens' PeopleConnectionsList (Maybe Text)
pclFields
  = lens _pclFields (\ s a -> s{_pclFields = a})

-- | JSONP
pclCallback :: Lens' PeopleConnectionsList (Maybe Text)
pclCallback
  = lens _pclCallback (\ s a -> s{_pclCallback = a})

instance GoogleRequest PeopleConnectionsList where
        type Rs PeopleConnectionsList =
             ListConnectionsResponse
        type Scopes PeopleConnectionsList =
             '["https://www.googleapis.com/auth/contacts",
               "https://www.googleapis.com/auth/contacts.readonly"]
        requestClient PeopleConnectionsList'{..}
          = go _pclResourceName _pclSyncToken _pclXgafv
              _pclUploadProtocol
              _pclRequestMaskIncludeField
              _pclRequestSyncToken
              (Just _pclPp)
              _pclAccessToken
              _pclUploadType
              _pclSortOrder
              _pclPersonFields
              _pclBearerToken
              _pclPageToken
              _pclPageSize
              _pclCallback
              _pclFields
              (Just AltJSON)
              peopleService
          where go
                  = buildClient
                      (Proxy :: Proxy PeopleConnectionsListResource)
                      mempty
