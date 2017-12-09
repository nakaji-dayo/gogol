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
-- Module      : Network.Google.Resource.Classroom.Courses.CourseWork.StudentSubmissions.TurnIn
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns in a student submission. Turning in a student submission transfers
-- ownership of attached Drive files to the teacher and may also update the
-- submission state. This may only be called by the student that owns the
-- specified student submission. This request must be made by the Developer
-- Console project of the [OAuth client
-- ID](https:\/\/support.google.com\/cloud\/answer\/6158849) used to create
-- the corresponding course work item. This method returns the following
-- error codes: * \`PERMISSION_DENIED\` if the requesting user is not
-- permitted to access the requested course or course work, turn in the
-- requested student submission, or for access errors. *
-- \`INVALID_ARGUMENT\` if the request is malformed. * \`NOT_FOUND\` if the
-- requested course, course work, or student submission does not exist.
--
-- /See:/ <https://developers.google.com/classroom/ Google Classroom API Reference> for @classroom.courses.courseWork.studentSubmissions.turnIn@.
module Network.Google.Resource.Classroom.Courses.CourseWork.StudentSubmissions.TurnIn
    (
    -- * REST Resource
      CoursesCourseWorkStudentSubmissionsTurnInResource

    -- * Creating a Request
    , coursesCourseWorkStudentSubmissionsTurnIn
    , CoursesCourseWorkStudentSubmissionsTurnIn

    -- * Request Lenses
    , ccwsstiXgafv
    , ccwsstiUploadProtocol
    , ccwsstiPp
    , ccwsstiCourseId
    , ccwsstiAccessToken
    , ccwsstiUploadType
    , ccwsstiPayload
    , ccwsstiBearerToken
    , ccwsstiId
    , ccwsstiFields
    , ccwsstiCallback
    , ccwsstiCourseWorkId
    ) where

import Network.Google.Classroom.Types
import Network.Google.Prelude

-- | A resource alias for @classroom.courses.courseWork.studentSubmissions.turnIn@ method which the
-- 'CoursesCourseWorkStudentSubmissionsTurnIn' request conforms to.
type CoursesCourseWorkStudentSubmissionsTurnInResource
     =
     "v1" :>
       "courses" :>
         Capture "courseId" Text :>
           "courseWork" :>
             Capture "courseWorkId" Text :>
               "studentSubmissions" :>
                 CaptureMode "id" "turnIn" Text :>
                   QueryParam "$.xgafv" Xgafv :>
                     QueryParam "upload_protocol" Text :>
                       QueryParam "pp" Bool :>
                         QueryParam "access_token" Text :>
                           QueryParam "uploadType" Text :>
                             QueryParam "bearer_token" Text :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     ReqBody '[JSON]
                                       TurnInStudentSubmissionRequest
                                       :> Post '[JSON] Empty

-- | Turns in a student submission. Turning in a student submission transfers
-- ownership of attached Drive files to the teacher and may also update the
-- submission state. This may only be called by the student that owns the
-- specified student submission. This request must be made by the Developer
-- Console project of the [OAuth client
-- ID](https:\/\/support.google.com\/cloud\/answer\/6158849) used to create
-- the corresponding course work item. This method returns the following
-- error codes: * \`PERMISSION_DENIED\` if the requesting user is not
-- permitted to access the requested course or course work, turn in the
-- requested student submission, or for access errors. *
-- \`INVALID_ARGUMENT\` if the request is malformed. * \`NOT_FOUND\` if the
-- requested course, course work, or student submission does not exist.
--
-- /See:/ 'coursesCourseWorkStudentSubmissionsTurnIn' smart constructor.
data CoursesCourseWorkStudentSubmissionsTurnIn = CoursesCourseWorkStudentSubmissionsTurnIn'
    { _ccwsstiXgafv :: !(Maybe Xgafv)
    , _ccwsstiUploadProtocol :: !(Maybe Text)
    , _ccwsstiPp :: !Bool
    , _ccwsstiCourseId :: !Text
    , _ccwsstiAccessToken :: !(Maybe Text)
    , _ccwsstiUploadType :: !(Maybe Text)
    , _ccwsstiPayload :: !TurnInStudentSubmissionRequest
    , _ccwsstiBearerToken :: !(Maybe Text)
    , _ccwsstiId :: !Text
    , _ccwsstiFields :: !(Maybe Text)
    , _ccwsstiCallback :: !(Maybe Text)
    , _ccwsstiCourseWorkId :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CoursesCourseWorkStudentSubmissionsTurnIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccwsstiXgafv'
--
-- * 'ccwsstiUploadProtocol'
--
-- * 'ccwsstiPp'
--
-- * 'ccwsstiCourseId'
--
-- * 'ccwsstiAccessToken'
--
-- * 'ccwsstiUploadType'
--
-- * 'ccwsstiPayload'
--
-- * 'ccwsstiBearerToken'
--
-- * 'ccwsstiId'
--
-- * 'ccwsstiFields'
--
-- * 'ccwsstiCallback'
--
-- * 'ccwsstiCourseWorkId'
coursesCourseWorkStudentSubmissionsTurnIn
    :: Text -- ^ 'ccwsstiCourseId'
    -> TurnInStudentSubmissionRequest -- ^ 'ccwsstiPayload'
    -> Text -- ^ 'ccwsstiId'
    -> Text -- ^ 'ccwsstiCourseWorkId'
    -> CoursesCourseWorkStudentSubmissionsTurnIn
coursesCourseWorkStudentSubmissionsTurnIn pCcwsstiCourseId_ pCcwsstiPayload_ pCcwsstiId_ pCcwsstiCourseWorkId_ = 
    CoursesCourseWorkStudentSubmissionsTurnIn'
    { _ccwsstiXgafv = Nothing
    , _ccwsstiUploadProtocol = Nothing
    , _ccwsstiPp = True
    , _ccwsstiCourseId = pCcwsstiCourseId_
    , _ccwsstiAccessToken = Nothing
    , _ccwsstiUploadType = Nothing
    , _ccwsstiPayload = pCcwsstiPayload_
    , _ccwsstiBearerToken = Nothing
    , _ccwsstiId = pCcwsstiId_
    , _ccwsstiFields = Nothing
    , _ccwsstiCallback = Nothing
    , _ccwsstiCourseWorkId = pCcwsstiCourseWorkId_
    }

-- | V1 error format.
ccwsstiXgafv :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Xgafv)
ccwsstiXgafv
  = lens _ccwsstiXgafv (\ s a -> s{_ccwsstiXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ccwsstiUploadProtocol :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Text)
ccwsstiUploadProtocol
  = lens _ccwsstiUploadProtocol
      (\ s a -> s{_ccwsstiUploadProtocol = a})

-- | Pretty-print response.
ccwsstiPp :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn Bool
ccwsstiPp
  = lens _ccwsstiPp (\ s a -> s{_ccwsstiPp = a})

-- | Identifier of the course. This identifier can be either the
-- Classroom-assigned identifier or an alias.
ccwsstiCourseId :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn Text
ccwsstiCourseId
  = lens _ccwsstiCourseId
      (\ s a -> s{_ccwsstiCourseId = a})

-- | OAuth access token.
ccwsstiAccessToken :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Text)
ccwsstiAccessToken
  = lens _ccwsstiAccessToken
      (\ s a -> s{_ccwsstiAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ccwsstiUploadType :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Text)
ccwsstiUploadType
  = lens _ccwsstiUploadType
      (\ s a -> s{_ccwsstiUploadType = a})

-- | Multipart request metadata.
ccwsstiPayload :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn TurnInStudentSubmissionRequest
ccwsstiPayload
  = lens _ccwsstiPayload
      (\ s a -> s{_ccwsstiPayload = a})

-- | OAuth bearer token.
ccwsstiBearerToken :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Text)
ccwsstiBearerToken
  = lens _ccwsstiBearerToken
      (\ s a -> s{_ccwsstiBearerToken = a})

-- | Identifier of the student submission.
ccwsstiId :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn Text
ccwsstiId
  = lens _ccwsstiId (\ s a -> s{_ccwsstiId = a})

-- | Selector specifying which fields to include in a partial response.
ccwsstiFields :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Text)
ccwsstiFields
  = lens _ccwsstiFields
      (\ s a -> s{_ccwsstiFields = a})

-- | JSONP
ccwsstiCallback :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn (Maybe Text)
ccwsstiCallback
  = lens _ccwsstiCallback
      (\ s a -> s{_ccwsstiCallback = a})

-- | Identifier of the course work.
ccwsstiCourseWorkId :: Lens' CoursesCourseWorkStudentSubmissionsTurnIn Text
ccwsstiCourseWorkId
  = lens _ccwsstiCourseWorkId
      (\ s a -> s{_ccwsstiCourseWorkId = a})

instance GoogleRequest
         CoursesCourseWorkStudentSubmissionsTurnIn where
        type Rs CoursesCourseWorkStudentSubmissionsTurnIn =
             Empty
        type Scopes CoursesCourseWorkStudentSubmissionsTurnIn
             =
             '["https://www.googleapis.com/auth/classroom.coursework.me"]
        requestClient
          CoursesCourseWorkStudentSubmissionsTurnIn'{..}
          = go _ccwsstiCourseId _ccwsstiCourseWorkId _ccwsstiId
              _ccwsstiXgafv
              _ccwsstiUploadProtocol
              (Just _ccwsstiPp)
              _ccwsstiAccessToken
              _ccwsstiUploadType
              _ccwsstiBearerToken
              _ccwsstiCallback
              _ccwsstiFields
              (Just AltJSON)
              _ccwsstiPayload
              classroomService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           CoursesCourseWorkStudentSubmissionsTurnInResource)
                      mempty
