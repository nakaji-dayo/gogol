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
-- Module      : Network.Google.Resource.Classroom.Courses.Announcements.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an announcement. This method returns the following error codes:
-- * \`PERMISSION_DENIED\` if the requesting user is not permitted to
-- access the requested course or announcement, or for access errors. *
-- \`INVALID_ARGUMENT\` if the request is malformed. * \`NOT_FOUND\` if the
-- requested course or announcement does not exist.
--
-- /See:/ <https://developers.google.com/classroom/ Google Classroom API Reference> for @classroom.courses.announcements.get@.
module Network.Google.Resource.Classroom.Courses.Announcements.Get
    (
    -- * REST Resource
      CoursesAnnouncementsGetResource

    -- * Creating a Request
    , coursesAnnouncementsGet
    , CoursesAnnouncementsGet

    -- * Request Lenses
    , cagXgafv
    , cagUploadProtocol
    , cagPp
    , cagCourseId
    , cagAccessToken
    , cagUploadType
    , cagBearerToken
    , cagId
    , cagFields
    , cagCallback
    ) where

import Network.Google.Classroom.Types
import Network.Google.Prelude

-- | A resource alias for @classroom.courses.announcements.get@ method which the
-- 'CoursesAnnouncementsGet' request conforms to.
type CoursesAnnouncementsGetResource =
     "v1" :>
       "courses" :>
         Capture "courseId" Text :>
           "announcements" :>
             Capture "id" Text :>
               QueryParam "$.xgafv" Xgafv :>
                 QueryParam "upload_protocol" Text :>
                   QueryParam "pp" Bool :>
                     QueryParam "access_token" Text :>
                       QueryParam "uploadType" Text :>
                         QueryParam "bearer_token" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] Announcement

-- | Returns an announcement. This method returns the following error codes:
-- * \`PERMISSION_DENIED\` if the requesting user is not permitted to
-- access the requested course or announcement, or for access errors. *
-- \`INVALID_ARGUMENT\` if the request is malformed. * \`NOT_FOUND\` if the
-- requested course or announcement does not exist.
--
-- /See:/ 'coursesAnnouncementsGet' smart constructor.
data CoursesAnnouncementsGet = CoursesAnnouncementsGet'
    { _cagXgafv :: !(Maybe Xgafv)
    , _cagUploadProtocol :: !(Maybe Text)
    , _cagPp :: !Bool
    , _cagCourseId :: !Text
    , _cagAccessToken :: !(Maybe Text)
    , _cagUploadType :: !(Maybe Text)
    , _cagBearerToken :: !(Maybe Text)
    , _cagId :: !Text
    , _cagFields :: !(Maybe Text)
    , _cagCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CoursesAnnouncementsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cagXgafv'
--
-- * 'cagUploadProtocol'
--
-- * 'cagPp'
--
-- * 'cagCourseId'
--
-- * 'cagAccessToken'
--
-- * 'cagUploadType'
--
-- * 'cagBearerToken'
--
-- * 'cagId'
--
-- * 'cagFields'
--
-- * 'cagCallback'
coursesAnnouncementsGet
    :: Text -- ^ 'cagCourseId'
    -> Text -- ^ 'cagId'
    -> CoursesAnnouncementsGet
coursesAnnouncementsGet pCagCourseId_ pCagId_ = 
    CoursesAnnouncementsGet'
    { _cagXgafv = Nothing
    , _cagUploadProtocol = Nothing
    , _cagPp = True
    , _cagCourseId = pCagCourseId_
    , _cagAccessToken = Nothing
    , _cagUploadType = Nothing
    , _cagBearerToken = Nothing
    , _cagId = pCagId_
    , _cagFields = Nothing
    , _cagCallback = Nothing
    }

-- | V1 error format.
cagXgafv :: Lens' CoursesAnnouncementsGet (Maybe Xgafv)
cagXgafv = lens _cagXgafv (\ s a -> s{_cagXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
cagUploadProtocol :: Lens' CoursesAnnouncementsGet (Maybe Text)
cagUploadProtocol
  = lens _cagUploadProtocol
      (\ s a -> s{_cagUploadProtocol = a})

-- | Pretty-print response.
cagPp :: Lens' CoursesAnnouncementsGet Bool
cagPp = lens _cagPp (\ s a -> s{_cagPp = a})

-- | Identifier of the course. This identifier can be either the
-- Classroom-assigned identifier or an alias.
cagCourseId :: Lens' CoursesAnnouncementsGet Text
cagCourseId
  = lens _cagCourseId (\ s a -> s{_cagCourseId = a})

-- | OAuth access token.
cagAccessToken :: Lens' CoursesAnnouncementsGet (Maybe Text)
cagAccessToken
  = lens _cagAccessToken
      (\ s a -> s{_cagAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
cagUploadType :: Lens' CoursesAnnouncementsGet (Maybe Text)
cagUploadType
  = lens _cagUploadType
      (\ s a -> s{_cagUploadType = a})

-- | OAuth bearer token.
cagBearerToken :: Lens' CoursesAnnouncementsGet (Maybe Text)
cagBearerToken
  = lens _cagBearerToken
      (\ s a -> s{_cagBearerToken = a})

-- | Identifier of the announcement.
cagId :: Lens' CoursesAnnouncementsGet Text
cagId = lens _cagId (\ s a -> s{_cagId = a})

-- | Selector specifying which fields to include in a partial response.
cagFields :: Lens' CoursesAnnouncementsGet (Maybe Text)
cagFields
  = lens _cagFields (\ s a -> s{_cagFields = a})

-- | JSONP
cagCallback :: Lens' CoursesAnnouncementsGet (Maybe Text)
cagCallback
  = lens _cagCallback (\ s a -> s{_cagCallback = a})

instance GoogleRequest CoursesAnnouncementsGet where
        type Rs CoursesAnnouncementsGet = Announcement
        type Scopes CoursesAnnouncementsGet =
             '["https://www.googleapis.com/auth/classroom.announcements",
               "https://www.googleapis.com/auth/classroom.announcements.readonly"]
        requestClient CoursesAnnouncementsGet'{..}
          = go _cagCourseId _cagId _cagXgafv _cagUploadProtocol
              (Just _cagPp)
              _cagAccessToken
              _cagUploadType
              _cagBearerToken
              _cagCallback
              _cagFields
              (Just AltJSON)
              classroomService
          where go
                  = buildClient
                      (Proxy :: Proxy CoursesAnnouncementsGetResource)
                      mempty
