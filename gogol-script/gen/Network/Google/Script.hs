{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Script
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An API for managing and executing Google Apps Script projects.
--
-- /See:/ <https://developers.google.com/apps-script/execution/rest/v1/scripts/run Google Apps Script API Reference>
module Network.Google.Script
    (
    -- * Service Configuration
      scriptService

    -- * OAuth Scopes
    , mailGoogleComScope
    , m8FeedsScope
    , adminDirectoryUserScope
    , userInfoEmailScope
    , formsCurrentOnlyScope
    , driveScope
    , adminDirectoryGroupScope
    , calendarFeedsScope
    , formsScope
    , spreadsheetsScope
    , groupsScope

    -- * API Declaration
    , ScriptAPI

    -- * Resources

    -- ** script.scripts.run
    , module Network.Google.Resource.Script.Scripts.Run

    -- * Types

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse

    -- ** ExecutionRequest
    , ExecutionRequest
    , executionRequest
    , erFunction
    , erSessionState
    , erDevMode
    , erParameters

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** ScriptStackTraceElement
    , ScriptStackTraceElement
    , scriptStackTraceElement
    , ssteFunction
    , ssteLineNumber

    -- ** Xgafv
    , Xgafv (..)

    -- ** ExecutionError
    , ExecutionError
    , executionError
    , eeScriptStackTraceElements
    , eeErrorType
    , eeErrorMessage

    -- ** OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional

    -- ** ExecutionResponse
    , ExecutionResponse
    , executionResponse
    , erResult
    ) where

import Network.Google.Prelude
import Network.Google.Resource.Script.Scripts.Run
import Network.Google.Script.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Apps Script API service.
type ScriptAPI = ScriptsRunResource
