module Qi.AWS.Renderable where

import Qi.AWS.Types
import Stratosphere (Resource)

class AwsResource a => Renderable a where
  render :: AppName -> (LogicalId (ResourceType a), a) -> Resource
