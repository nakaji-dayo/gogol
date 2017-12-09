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
-- Module      : Network.Google.Resource.IAM.Projects.Roles.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Role definition.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.roles.get@.
module Network.Google.Resource.IAM.Projects.Roles.Get
    (
    -- * REST Resource
      ProjectsRolesGetResource

    -- * Creating a Request
    , projectsRolesGet
    , ProjectsRolesGet

    -- * Request Lenses
    , prgXgafv
    , prgUploadProtocol
    , prgPp
    , prgAccessToken
    , prgUploadType
    , prgBearerToken
    , prgName
    , prgFields
    , prgCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.roles.get@ method which the
-- 'ProjectsRolesGet' request conforms to.
type ProjectsRolesGetResource =
     "v1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] Role

-- | Gets a Role definition.
--
-- /See:/ 'projectsRolesGet' smart constructor.
data ProjectsRolesGet = ProjectsRolesGet'
    { _prgXgafv :: !(Maybe Xgafv)
    , _prgUploadProtocol :: !(Maybe Text)
    , _prgPp :: !Bool
    , _prgAccessToken :: !(Maybe Text)
    , _prgUploadType :: !(Maybe Text)
    , _prgBearerToken :: !(Maybe Text)
    , _prgName :: !Text
    , _prgFields :: !(Maybe Text)
    , _prgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsRolesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prgXgafv'
--
-- * 'prgUploadProtocol'
--
-- * 'prgPp'
--
-- * 'prgAccessToken'
--
-- * 'prgUploadType'
--
-- * 'prgBearerToken'
--
-- * 'prgName'
--
-- * 'prgFields'
--
-- * 'prgCallback'
projectsRolesGet
    :: Text -- ^ 'prgName'
    -> ProjectsRolesGet
projectsRolesGet pPrgName_ = 
    ProjectsRolesGet'
    { _prgXgafv = Nothing
    , _prgUploadProtocol = Nothing
    , _prgPp = True
    , _prgAccessToken = Nothing
    , _prgUploadType = Nothing
    , _prgBearerToken = Nothing
    , _prgName = pPrgName_
    , _prgFields = Nothing
    , _prgCallback = Nothing
    }

-- | V1 error format.
prgXgafv :: Lens' ProjectsRolesGet (Maybe Xgafv)
prgXgafv = lens _prgXgafv (\ s a -> s{_prgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prgUploadProtocol :: Lens' ProjectsRolesGet (Maybe Text)
prgUploadProtocol
  = lens _prgUploadProtocol
      (\ s a -> s{_prgUploadProtocol = a})

-- | Pretty-print response.
prgPp :: Lens' ProjectsRolesGet Bool
prgPp = lens _prgPp (\ s a -> s{_prgPp = a})

-- | OAuth access token.
prgAccessToken :: Lens' ProjectsRolesGet (Maybe Text)
prgAccessToken
  = lens _prgAccessToken
      (\ s a -> s{_prgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prgUploadType :: Lens' ProjectsRolesGet (Maybe Text)
prgUploadType
  = lens _prgUploadType
      (\ s a -> s{_prgUploadType = a})

-- | OAuth bearer token.
prgBearerToken :: Lens' ProjectsRolesGet (Maybe Text)
prgBearerToken
  = lens _prgBearerToken
      (\ s a -> s{_prgBearerToken = a})

-- | The resource name of the role in one of the following formats:
-- \`roles\/{ROLE_NAME}\`
-- \`organizations\/{ORGANIZATION_ID}\/roles\/{ROLE_NAME}\`
-- \`projects\/{PROJECT_ID}\/roles\/{ROLE_NAME}\`
prgName :: Lens' ProjectsRolesGet Text
prgName = lens _prgName (\ s a -> s{_prgName = a})

-- | Selector specifying which fields to include in a partial response.
prgFields :: Lens' ProjectsRolesGet (Maybe Text)
prgFields
  = lens _prgFields (\ s a -> s{_prgFields = a})

-- | JSONP
prgCallback :: Lens' ProjectsRolesGet (Maybe Text)
prgCallback
  = lens _prgCallback (\ s a -> s{_prgCallback = a})

instance GoogleRequest ProjectsRolesGet where
        type Rs ProjectsRolesGet = Role
        type Scopes ProjectsRolesGet =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsRolesGet'{..}
          = go _prgName _prgXgafv _prgUploadProtocol
              (Just _prgPp)
              _prgAccessToken
              _prgUploadType
              _prgBearerToken
              _prgCallback
              _prgFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsRolesGetResource)
                      mempty
