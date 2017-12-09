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
-- Module      : Network.Google.Resource.Classroom.Courses.Announcements.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more fields of an announcement. This method returns the
-- following error codes: * \`PERMISSION_DENIED\` if the requesting
-- developer project did not create the corresponding announcement or for
-- access errors. * \`INVALID_ARGUMENT\` if the request is malformed. *
-- \`FAILED_PRECONDITION\` if the requested announcement has already been
-- deleted. * \`NOT_FOUND\` if the requested course or announcement does
-- not exist
--
-- /See:/ <https://developers.google.com/classroom/ Google Classroom API Reference> for @classroom.courses.announcements.patch@.
module Network.Google.Resource.Classroom.Courses.Announcements.Patch
    (
    -- * REST Resource
      CoursesAnnouncementsPatchResource

    -- * Creating a Request
    , coursesAnnouncementsPatch
    , CoursesAnnouncementsPatch

    -- * Request Lenses
    , capXgafv
    , capUploadProtocol
    , capUpdateMask
    , capPp
    , capCourseId
    , capAccessToken
    , capUploadType
    , capPayload
    , capBearerToken
    , capId
    , capFields
    , capCallback
    ) where

import Network.Google.Classroom.Types
import Network.Google.Prelude

-- | A resource alias for @classroom.courses.announcements.patch@ method which the
-- 'CoursesAnnouncementsPatch' request conforms to.
type CoursesAnnouncementsPatchResource =
     "v1" :>
       "courses" :>
         Capture "courseId" Text :>
           "announcements" :>
             Capture "id" Text :>
               QueryParam "$.xgafv" Xgafv :>
                 QueryParam "upload_protocol" Text :>
                   QueryParam "updateMask" FieldMask :>
                     QueryParam "pp" Bool :>
                       QueryParam "access_token" Text :>
                         QueryParam "uploadType" Text :>
                           QueryParam "bearer_token" Text :>
                             QueryParam "callback" Text :>
                               QueryParam "fields" Text :>
                                 QueryParam "alt" AltJSON :>
                                   ReqBody '[JSON] Announcement :>
                                     Patch '[JSON] Announcement

-- | Updates one or more fields of an announcement. This method returns the
-- following error codes: * \`PERMISSION_DENIED\` if the requesting
-- developer project did not create the corresponding announcement or for
-- access errors. * \`INVALID_ARGUMENT\` if the request is malformed. *
-- \`FAILED_PRECONDITION\` if the requested announcement has already been
-- deleted. * \`NOT_FOUND\` if the requested course or announcement does
-- not exist
--
-- /See:/ 'coursesAnnouncementsPatch' smart constructor.
data CoursesAnnouncementsPatch = CoursesAnnouncementsPatch'
    { _capXgafv :: !(Maybe Xgafv)
    , _capUploadProtocol :: !(Maybe Text)
    , _capUpdateMask :: !(Maybe FieldMask)
    , _capPp :: !Bool
    , _capCourseId :: !Text
    , _capAccessToken :: !(Maybe Text)
    , _capUploadType :: !(Maybe Text)
    , _capPayload :: !Announcement
    , _capBearerToken :: !(Maybe Text)
    , _capId :: !Text
    , _capFields :: !(Maybe Text)
    , _capCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CoursesAnnouncementsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capXgafv'
--
-- * 'capUploadProtocol'
--
-- * 'capUpdateMask'
--
-- * 'capPp'
--
-- * 'capCourseId'
--
-- * 'capAccessToken'
--
-- * 'capUploadType'
--
-- * 'capPayload'
--
-- * 'capBearerToken'
--
-- * 'capId'
--
-- * 'capFields'
--
-- * 'capCallback'
coursesAnnouncementsPatch
    :: Text -- ^ 'capCourseId'
    -> Announcement -- ^ 'capPayload'
    -> Text -- ^ 'capId'
    -> CoursesAnnouncementsPatch
coursesAnnouncementsPatch pCapCourseId_ pCapPayload_ pCapId_ = 
    CoursesAnnouncementsPatch'
    { _capXgafv = Nothing
    , _capUploadProtocol = Nothing
    , _capUpdateMask = Nothing
    , _capPp = True
    , _capCourseId = pCapCourseId_
    , _capAccessToken = Nothing
    , _capUploadType = Nothing
    , _capPayload = pCapPayload_
    , _capBearerToken = Nothing
    , _capId = pCapId_
    , _capFields = Nothing
    , _capCallback = Nothing
    }

-- | V1 error format.
capXgafv :: Lens' CoursesAnnouncementsPatch (Maybe Xgafv)
capXgafv = lens _capXgafv (\ s a -> s{_capXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
capUploadProtocol :: Lens' CoursesAnnouncementsPatch (Maybe Text)
capUploadProtocol
  = lens _capUploadProtocol
      (\ s a -> s{_capUploadProtocol = a})

-- | Mask that identifies which fields on the announcement to update. This
-- field is required to do an update. The update fails if invalid fields
-- are specified. If a field supports empty values, it can be cleared by
-- specifying it in the update mask and not in the Announcement object. If
-- a field that does not support empty values is included in the update
-- mask and not set in the Announcement object, an \`INVALID_ARGUMENT\`
-- error will be returned. The following fields may be specified by
-- teachers: * \`text\` * \`state\` * \`scheduled_time\`
capUpdateMask :: Lens' CoursesAnnouncementsPatch (Maybe FieldMask)
capUpdateMask
  = lens _capUpdateMask
      (\ s a -> s{_capUpdateMask = a})

-- | Pretty-print response.
capPp :: Lens' CoursesAnnouncementsPatch Bool
capPp = lens _capPp (\ s a -> s{_capPp = a})

-- | Identifier of the course. This identifier can be either the
-- Classroom-assigned identifier or an alias.
capCourseId :: Lens' CoursesAnnouncementsPatch Text
capCourseId
  = lens _capCourseId (\ s a -> s{_capCourseId = a})

-- | OAuth access token.
capAccessToken :: Lens' CoursesAnnouncementsPatch (Maybe Text)
capAccessToken
  = lens _capAccessToken
      (\ s a -> s{_capAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
capUploadType :: Lens' CoursesAnnouncementsPatch (Maybe Text)
capUploadType
  = lens _capUploadType
      (\ s a -> s{_capUploadType = a})

-- | Multipart request metadata.
capPayload :: Lens' CoursesAnnouncementsPatch Announcement
capPayload
  = lens _capPayload (\ s a -> s{_capPayload = a})

-- | OAuth bearer token.
capBearerToken :: Lens' CoursesAnnouncementsPatch (Maybe Text)
capBearerToken
  = lens _capBearerToken
      (\ s a -> s{_capBearerToken = a})

-- | Identifier of the announcement.
capId :: Lens' CoursesAnnouncementsPatch Text
capId = lens _capId (\ s a -> s{_capId = a})

-- | Selector specifying which fields to include in a partial response.
capFields :: Lens' CoursesAnnouncementsPatch (Maybe Text)
capFields
  = lens _capFields (\ s a -> s{_capFields = a})

-- | JSONP
capCallback :: Lens' CoursesAnnouncementsPatch (Maybe Text)
capCallback
  = lens _capCallback (\ s a -> s{_capCallback = a})

instance GoogleRequest CoursesAnnouncementsPatch
         where
        type Rs CoursesAnnouncementsPatch = Announcement
        type Scopes CoursesAnnouncementsPatch =
             '["https://www.googleapis.com/auth/classroom.announcements"]
        requestClient CoursesAnnouncementsPatch'{..}
          = go _capCourseId _capId _capXgafv _capUploadProtocol
              _capUpdateMask
              (Just _capPp)
              _capAccessToken
              _capUploadType
              _capBearerToken
              _capCallback
              _capFields
              (Just AltJSON)
              _capPayload
              classroomService
          where go
                  = buildClient
                      (Proxy :: Proxy CoursesAnnouncementsPatchResource)
                      mempty
