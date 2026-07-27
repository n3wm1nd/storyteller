-- | The newtype-wrapped 'Storyteller.Context.DSL.Rendering.Context'
--   shapes an agent actually takes as a parameter, one per distinct
--   purpose -- kept in their own module (not
--   "Storyteller.Writer.Agent", the base shared-vocabulary module)
--   because "Storyteller.Context.DSL.Rendering" transitively imports
--   "Storyteller.Context.DSL.Render", which already imports
--   "Storyteller.Writer.Agent" for 'Storyteller.Writer.Agent.ContextBlock'\/
--   'Storyteller.Writer.Agent.renderEmbeddedFile" -- putting these
--   newtypes there too would be a module cycle.
--
--   Each wraps the identical underlying 'Storyteller.Context.DSL.Rendering.Context'
--   tree; the newtype is purely a label distinguishing "world context" from
--   "style" from "pinned/short-term context" at a call site and in a type
--   signature, the same reason 'Storyteller.Writer.Agent.CharLabel' wraps
--   plain 'Data.Text.Text'. An agent receiving one of these renders it
--   itself, at the point it builds its own LLM call
--   ('Storyteller.Context.DSL.Rendering.renderMessages'\/
--   'Storyteller.Context.DSL.Rendering.renderText'), rather than receiving
--   already-flattened @['UniversalLLM.Message']@\/'Storyteller.Writer.Agent.ContextBlock's
--   the way it used to -- see "Storyteller.Writer.Agent.Write"'s own
--   Haddock for why that move matters (rendering now happens where the
--   model and budget are actually known, not upstream in
--   "Server.Writer.File").
module Storyteller.Writer.Agent.Context
  ( SceneContext(..)
  , StyleContext(..)
  , PinnedContext(..)
  , CharacterContext(..)
  , ProgramContext(..)
  , Lore(..)
  , Other(..)
  ) where

import Storyteller.Context.DSL.Rendering (Context)
import Storyteller.Context.DSL.Value (Message)

-- $rendered
--
-- All but 'CharacterContext' carry already-rendered
-- @['Storyteller.Context.DSL.Value.Message']@ rather than the unforced
-- 'Context' tree: whoever dispatches has already run the DSL and chosen a
-- traversal (almost always
-- 'Storyteller.Context.DSL.Rendering.contextOwnMessages' -- see its
-- Haddock for why walking children too usually double-counts). An agent
-- receiving one of these needs no storage capability to use it, and cannot
-- re-resolve mid-run: what it gets is what the user previewed.
--
-- 'Message', not 'Data.Text.Text', because the role is real content.
-- 'Storyteller.Context.DSL.Library.chapterEntryDef' is @> read f@, so a
-- chapter is an @Assistant@ turn -- prior prose framed as the model's own.
-- Flattening to text here would demote it, and would also collapse the
-- message boundaries a provider's prompt cache keys on.

-- | The scene a roleplay turn happens in -- resolved @context.writer@ for
--   the file being written, which is existing prose plus whatever lore and
--   other material that definition pulls in.
--
--   Named for the scene, not "world", because that is what it is and the
--   only agent that takes one is
--   'Storyteller.Writer.Agent.Roleplay.roleplayAgent'. "World context" said
--   nothing: it described neither where the content came from nor what the
--   receiving agent does with it, and invited exactly the mistake
--   'Lore''s own Haddock warns about -- bundling caller-supplied lore
--   together with agent-derived material into one anonymous blob.
newtype SceneContext = SceneContext [Message]

newtype StyleContext = StyleContext [Message]

newtype PinnedContext = PinnedContext [Message]

-- | One character's own resolved @context.character@ tree, buckets intact
--   (@"sheet"@\/@"full"@\/@"journal"@\/@"journalFull"@ -- reached with
--   'Storyteller.Context.DSL.Rendering.namedChild', a real structural
--   lookup rather than string-matching a flat list).
--
--   Supplied already-resolved by whoever is dispatching, the same as
--   'Lore' and 'Other': *which* character to ask about is the caller's
--   decision, and resolving @context.character@ is Context DSL assembly,
--   not something an agent should be doing between deciding what to ask
--   and asking it. What stays with the agent is picking buckets and
--   rendering, both pure.
--
--   The one wrapper still holding a 'Context' rather than a flat message
--   list, because it is the one an agent genuinely /projects/: the buckets
--   are the point, and flattening would destroy them.
newtype CharacterContext = CharacterContext Context

-- | A user-defined agent's own program output, already resolved -- see
--   'Storyteller.Writer.Agent.Custom.customAgent'. Distinct from every
--   other wrapper here because it isn't a slot a built-in agent's Haskell
--   decided to consult: it /is/ that agent's entire definition, named by
--   slug ('Storyteller.Writer.Agent.Custom.customContextName') and
--   resolved by whoever dispatches.
newtype ProgramContext = ProgramContext [Message]

-- | The one user-influenceable slot 'Storyteller.Writer.Agent.Write.writeAgent'
--   itself can't derive on its own -- which lore is *relevant* to this call
--   is a judgment only a caller (a client's own @context.lore@ override, or
--   nothing, meaning the compiled-in default) can make; everything else
--   'writeAgent' wants (earlier chapters, who's present, their own
--   context) it reads for itself, from @path@ and the branch, no
--   parameter needed. See 'writeAgent's own Haddock. Deliberately its own
--   type rather than bundled with chapters\/other\/style: those are
--   agent-derived, this is caller-supplied, and conflating the two in one
--   anonymous blob is exactly the mistake this newtype exists to avoid
--   repeating.
newtype Lore = Lore [Message]

-- | 'Lore''s own twin for @context.other@ -- the catch-all "loose notes and
--   drafts" bucket (anything not lore\/chapters\/style.md\/chat scratch).
--   Which "other" files are relevant to this call is the same kind of
--   judgment 'Lore' already carries, so it gets the identical treatment:
--   a caller (typically 'Server.Writer.File.chatWriter', resolving a
--   client's own @context.other@ override or the compiled-in default)
--   supplies it, already resolved, rather than 'writeAgent' resolving
--   @context.other@ internally the way it used to.
newtype Other = Other [Message]
