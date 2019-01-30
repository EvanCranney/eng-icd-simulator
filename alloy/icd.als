// ============================================================================
// SWEN90010 2018 - Assignment 3 Submission
// by ecranney & sweerakoon
// ============================================================================

module ebs
open util/ordering[State] as ord

// =========================== System State ===================================
// a type for storing amounts of Joules
sig Joules {}

// the initial number of joules to deliver (30)
one sig InitialJoulesToDeliver extends Joules {}

// we ignore the clinical assistants for simplicity in this model 
abstract sig Role {}
one sig Cardiologist, Patient extends Role {}

// principals have associated roles
sig Principal {
  roles : set Role
}

// an abstract signature for network messages
abstract sig Message {
  source : Principal
}

// ChangeSettingsRequest messages
// Note: we ignore the tachybound part to keep things tractable
sig ChangeSettingsMessage extends Message {
  joules_to_deliver : Joules
}

// ModeOn message
sig ModeOnMessage extends Message {}

// Modes: either On or Off
abstract sig Mode {}
one sig ModeOn, ModeOff extends Mode {}

// meta information in the model that identifies the last action performed
abstract sig Action {
  who : Principal  // indentifies which principal caused the action
}

sig SendModeOn, RecvModeOn,
    SendChangeSettings, RecvChangeSettings
    extends Action {}

// represents the occurrence of attacker actions
one sig AttackerAction extends Action {}

// a dummy action which will be the "last action" in the initial state
// we do this just to make sure that every state has a last action
one sig DummyInitialAction extends Action {}

// The system state
sig State {
  network : lone Message,       // CAN Bus state: holds up to one message
  icd_mode : Mode,              // whether ICD system is in on or off mode
  impulse_mode : Mode,          // whether impulse generator is on or off
  joules_to_deliver : Joules,   // joules to deliver for vent. fibrillation
  authorised_card : Principal,  // the authorised cardiologist
  last_action : Action,         // identifies most recent action performed
}

// an axiom that restricts the model to never allow more than one message on
// the network at a time; a simplifying assumption to ease the analysis
fact {
  all s : State | lone s.network
}

// =========================== Initial State ==================================

// The initial state of the system:
// - empty network, 
// - ICD and impulse generator both off
// - joules to deliver at initial value
// - the authorised cardiologist is really a cardiologist
// - last_action set to the dummy value
pred Init[s : State] {
  no s.network and
  s.icd_mode = ModeOff and
  s.impulse_mode = ModeOff and
  s.joules_to_deliver = InitialJoulesToDeliver and 
  Cardiologist in s.authorised_card.roles and
  s.last_action = DummyInitialAction
}

// =========================== Actions ========================================

// Models the action in which a ModeOn message is sent on the network by the
// authorised cardiologist.
//
// Precondition:
// - none
//
// Postcondition:
// - network contains ModeOn message from the authorised cardiologist
// - last_action is SendModeOn (for the message's sender)
//      - last_action.who = sender of the message
// - no other changes
//
pred send_mode_on[s, s' : State] {
  some m : ModeOnMessage | m.source = s.authorised_card and
  s'.network = s.network + m and
  s'.icd_mode = s.icd_mode and
  s'.impulse_mode = s.impulse_mode and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.authorised_card = s.authorised_card and
  s'.last_action in SendModeOn and
  s'.last_action.who = m.source
}

// Models the action in which a valid ModeOn message is received by the ICD
// from the authorised cardiologist, causing the ICD system's mode to change
// from Off to On and the message to be removed from the network.
// 
// Precondition: <FILL IN HERE>
// - network contains ModeOn message from the authorised cardiologist
// - icd_mode in ModeOff
// - impulse_mode in ModeOff
//
// Postcondition: <FILL IN HERE>
// - network contains no message
// - icd_mode in ModeOn
// - impulse_mode in ModeOn
// - last_action in RecvModeOn (for the sender of the ModeOn message)
//      - last_action.who = source of the message
// - no other changes
//
pred recv_mode_on[s, s' : State] {
  // Preconditions:
  some m : ModeOnMessage | m.source = s.authorised_card and
  s.network = m and
  s.icd_mode in ModeOff and
  s.impulse_mode in ModeOff and

  // Postconditions:
  no s'.network and
  s'.icd_mode in ModeOn and
  s'.impulse_mode in ModeOn and
  s'.last_action in RecvModeOn and

  s'.authorised_card = s.authorised_card and
  s'.last_action.who = s.last_action.who and
  s'.joules_to_deliver = s.joules_to_deliver
}

// Models the action in which a valid ChangeSettingsRequest message is sent
// on the network from the authorised cardiologist, specifying the new
// quantity of joules to deliver for ventrical fibrillation.
//
// Precondition: <FILL IN HERE>
// - none
//
// Postcondition: <FILL IN HERE>
// - network contains ChangeSettingsMessage from the authorised cardiologist
// - last_action in SendChangeSettings (for the sender of the message)
//      - last_action.who = the source of the message
// - no other changes
//
pred send_change_settings[s, s' : State] {
  // Preconditions:
  some m : ChangeSettingsMessage | m.source = s.authorised_card and

  // Postconditions:
  s'.network = s.network + m and
  s'.last_action in SendChangeSettings and
  s'.last_action.who = m.source and

  // Unchanged:
  s'.icd_mode = s.icd_mode and 
  s'.impulse_mode = s.impulse_mode and
  s'.authorised_card = s.authorised_card and
  s'.joules_to_deliver = s.joules_to_deliver
}

// Models the action in which a valid ChangeSettingsRequest message is received
// by the ICD, from the authorised cardiologist, causing the current joules to
// be updated to that contained in the message and the message to be removed
// from the network.
//
// Precondition: <FILL IN HERE>
// - network contains ChangeSettingsMessage from authorised cardiologist
// - icd_mode in ModeOff
// - impulse_mode in ModeOff
//
// Postcondition: <FILL IN HERE>
// - network contains no message
// - joules_to_deliver = joules_to_deliver of the ChangeSettingsMessage
// - last_action in RecvChangeSettings (for the sender of the message)
//      - last_action.who = source of the message
// - no other changes
//
pred recv_change_settings[s, s' : State] {
  // Preconditions:
  some m : ChangeSettingsMessage | m.source = s.authorised_card and
  s.network = m and
  s.icd_mode in ModeOff and
  s.impulse_mode in ModeOff and
  //s.last_action in SendChangeSettings and
  //s.last_action.who = m.source and

  // Postconditions:
  no s'.network and
  s'.last_action in RecvChangeSettings and

  // Unchanged:
  s'.icd_mode = s.icd_mode and
  s'.impulse_mode = s.impulse_mode and
  s'.last_action.who = s.last_action.who and
  s'.authorised_card = s.authorised_card and
  s'.joules_to_deliver = m.joules_to_deliver
}

// =========================== Attacker Actions ==============================

// Models the actions of a potential attacker that has access to the network
// The only part of the system state that the attacker can possibly change
// is that of the network.

// UPDATED ATTACKER MODEL
//
// Atacker's Abilities:
// - Can modify the content and type of network messages that have already
//   been posted by an authorised cardiologist
// - Cannot fabricate new messages alleging to be from the authorised
//   cardiologist
// 
// Precodition:
// - network contains a message from the authorised cardiologist
//
// Postcondition:
// - network message type changes in accordance with attacker's abilities
//   (note, doesn't have to change, can be the same type)
// - last_action is AttackerAction
// - no other changes
//
pred attacker_action[s, s' : State] {
  // updated attacker action
  some m, m' : Message |
  m.source = s.authorised_card and
  m'.source = s.authorised_card and
  s.network = m and
  s'.network = m' and

  s'.icd_mode = s.icd_mode and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.impulse_mode = s.impulse_mode and
  s'.authorised_card = s.authorised_card and
  s'.last_action = AttackerAction
}


// ORIGINAL ATTACKER MODEL
//
// NOTE: In the initial template you are given, the attacker is modelled as
// being  able to modify the network contents arbitrarily. However, for later
// parts of the assignment you will change this definition to only permit
// certain kinds of modifications to the state of the network. When doing so,
// ensure you update the following line that describes the attacker's abilities.
//
// Attacker's abilities (ORIGINAL): <UPDATE HERE>
// - can modify network contents arbitrarily
//
// Precondition:
// - none
//
// Postcondition:
// - network state changes in accordance with attacker's abilities
// - last_action is AttackerAction
// - and nothing else changes
//
/*
pred attacker_action[s, s' : State] {
  s'.icd_mode = s.icd_mode and
  s'.joules_to_deliver = s.joules_to_deliver and
  s'.impulse_mode = s.impulse_mode and
  s'.authorised_card = s.authorised_card and
  s'.last_action = AttackerAction
}
*/

// =========================== State Transitions and Traces ===================

// State transitions occur via the various actions of the system above including
// those of the attacker.
pred state_transition[s, s' : State] {
  send_mode_on[s,s']
  or recv_mode_on[s,s']
  or send_change_settings[s,s']
  or recv_change_settings[s,s']
  or attacker_action[s,s']
}

// Define the linear ordering on states to be that generated by the state
// transitions above, defining execution traces to be sequences of states in
// which each state follows in the sequence from the last by a state transition.
fact state_transition_ord {
  all s: State, s': ord/next[s] {
    state_transition[s,s'] and s' != s
  }
}

// The initial state is first in the order, i.e. all execution traces that we
// model begin in the initial state described by the Init predicate
fact init_state {
  all s: ord/first {
    Init[s]
  }
}

// =========================== Properties =====================================

// Specifies that once the ICD is in the On mode, it never leaves the On mode
// in all future states in the execution trace, i.e. it stays in the On mode
// forever.
assert icd_never_off_after_on {
  all s : State | all s' : ord/nexts[s] | 
     s.icd_mode = ModeOn implies s'.icd_mode = ModeOn
}

check icd_never_off_after_on for 10 expect 0

// Describes a basic sanity condition of the system about how the modes of the
// ICD system and the impulse generator are related to each other. This
// condition should be true in all states of the system, i.e. it should be
// an "invariant"
pred inv[s : State] {
  s.icd_mode = s.impulse_mode
}

// Specifies that the invariant "inv" above should be true in all states of all
// execution traces of the system
assert inv_always {
  inv[ord/first] and all s : ord/nexts[ord/first] | inv[s]
  // NOTE (as a curiosity): the above is equivalent to saying
  // all s : State | inv[s]
  // This is because when checking this assertion, the linear order
  // defined on States causes all States considered by Alloy to come
  // from the linear order
}

// assertion checks over inv_always
check inv_always for 1  // holds
check inv_always for 3  // holds
check inv_always for 7  // holds
check inv_always for 15 // holds

// The invariant always holds.
//
// Every state transition (including both implementations of attacker_action)
// require icd_mode and impulse_mode to be the same.
//
// It is possible to articulate an informal inductive proof of why the invariant
// must hold:
//    (1) Base Case: the init[] predicate forces icd_mode and impulse_mode
//        to be the same (ModeOff) for the initial system state;
//    (2) Inductive Case: all subsequent transition predicates have post 
//        conditions which either (a) explicitly require icd_mode and
//        impulse_mode to be equal in the post-state, or (b) require the
//        post-state icd_mode and post-state impulse_mode to be equal to
//        their pre-state values.
//
// Since the pre-state for every transition predicate in the inductive case
// will either be the outgoing state from the init[] predicate or the
// post-state for some other transition predicate, it follows that there is
// no legal state in which icd_mode and impulse_mode can be nonequal.
//
// The init[] predicate forces icd_mode and impulse_mode to initialise in the
// same mode (ModeOff). Any subsequent non-attacker transition (send_mode_on[],
// recv_mode_on[], send_change_settings[], recv_change_settings[]) have 
// postconditions that explicitly force icd_mode and impulse_mode to be
// identical in the post-state. Both implementations of the attacker_action[]
// predicate require icd_mode and impulse_mode to be identical to their
// pre-state values. Since the pre-state for any transition predicate must be
// post-state for some other transition predicate (except init[]), since all
// since there is no state for which the post-state icd_mode and impulse_mode
// are non-equal, there can be no pre-states in which icd_mode and impulse_mode
// can be non-equal either.
//
// Note: the updated attacker model does not change the behaviour of this
// assertion; it changes neither the base case in which icd_mode and
// impulse_mode are both set to ModeOff, nor the inductive case in which
// every state transition requires either explicitly or implicitly requires
// icd_mode and impulse_mode to take the same value.

// An unexplained assertion.
//
// Provided that no attacks have occurred in the network, then any change to
// the ICD settings cannot have been caused by a principal who is a patient.
//
assert unexplained_assertion {
  all s : State |
      (all s' : State | s'.last_action not in AttackerAction)  // (1)
      => s.last_action in RecvChangeSettings                // (2)
      => Patient not in s.last_action.who.roles               // (3)
}

// assertion checks over unexplained_assertion
check unexplained_assertion for 1 but 2 State, 4 Action // holds
check unexplained_assertion for 1 but 3 State, 3 Action // holds
check unexplained_assertion for 1 but 3 State, 4 Action // fails
check unexplained_assertion for 2                       // holds
check unexplained_assertion for 3                       // holds
check unexplained_assertion for 4                       // fails
check unexplained_assertion for 7                       // fails
check unexplained_assertion for 10                      // fails

// The assertion does not always hold.
//
// The assertion does not hold for scopes of size four and above. This is
// because it is possible for the authorised cardiologist (i.e. the principal
// who is authorised to make changes to the ICDs mode and settings) to be both
// a Cardiologist and a Patient simultaneously.
// 
// Note that the assertion does not fail for scopes of size three and below.
// This is because there are - in those cases - insufficiently many instances
// of State and/or Action to create a counter-example.
//
// Note: the updated attacker model does not change the behaviour of this
// assertion. Since the assertion is conditioned on there being no state
// with AttackerAction as the last_action (i.e. no attacks having occurred),
// the fact that the updated attacker model still results in AttackerAction
// being the last_action in the resulting state means that it is - like the
// original attacker model - conditioned out of the assertion's set subspace
// by `s'.last_action not in AttackerAction`.

// Check that the device turns on only after properly instructed to, i.e. that
// the RecvModeOn action occurs only after a SendModeOn action has occurred.
assert turns_on_safe {
  all s, s' : State |
    (state_transition[s, s'] and s'.last_action in RecvModeOn)
    => (s.last_action in SendModeOn)
}

// assertion check for turns_on_safe
check turns_on_safe for 1
check turns_on_safe for 2
check turns_on_safe for 3
check turns_on_safe for 4
check turns_on_safe for 5 but 8 State
check turns_on_safe for 10

// <FILL IN HERE: does the assertion hold in the updated attacker model in which
// the attacker cannot guess Principal ids? why / why not?>
// what additional restrictions need to be added to the attacker model?
//
// The turns_on_safe assertion does not hold.
//
// This is because, even though an attacker can no longer create new messages
// on the network, they are still able to mutate messages that have been
// sent by the authorised cardiologist. In this case, the attacker is able to
// take ChangeSettingsMessages sent by the authorised cardiologist, and mutate
// them into ModeOnMessages. This will result in the ICD turning on (i.e. the
// occurence of the RecvModeOn action) without any valid request to do so
// (i.e. no SendModeOn action).
//
// Additional restrictions.
//
// To prevent the attacker from making these kinds of attacks, it would be
// necessary to prevent them from changing either the type of messages that
// are already on the network. This would be modelled by updating the
// attacker predicate to contain the additional condition:
//
//      `s.network = s'.network`
//
// In practice, this would probably be done by end-to-end encryption of
// messages between the sender (authorised cardiologist) and ICD, rather
// than just concealing the identity of source.


// Attacks still permitted by the updated attacker model:
// 
// There are two other types of attacks that are still possible with the
// restricted attacker model:
//   (2) Type Mutation - an attacker is able to mutate the type of messages
//       sent by the authorised cardiologist on the network. They may, for
//       example, mutate a ModeOnMessage into a ChangeSettingsMessage,
//       resulting in the ICD both failing to turn on and erroneously changing
//       its internal settings.
//   (3) Content Mutation - an attacker is able to mutate the payload of
//       messages sent by the authorised cardiologist on the network. They
//       could, for example, mutate the joules_to_deliver payload of a valid
//       ChangeSettingsMessage, resulting in an erroneous change in the
//       settings on the ICD.

// Relationship to our HAZOP study:
//
// The first attack is not covered by a design item / keyword in our HAZOP
// study. This modification is detailed below.
//
// (1) Attacker Mutates ChangeSettings into ModeOn Message
//     - Hazard: The ICD is on when it should be (and is expected to be) off.
//     - Design Item: If the ICD is off, when a ChangeSettingsRequest arrives
//       on the network from an authorised principal, then the ICD should
//       change its internal settings in accordance with the message, provided
//       that these would not set them outside of their safety thresholds.
//     - Keyword: OTHER THAN - the ICD's settings are unchanged, but the ICD is
//       instead switched to ModeOn.
//
// The other two attacks identified are, however, covered by design items /
// keywords in our HAZOP study. The details of these entries are detailed here.
//       
// (2) Attacker Mutates ModeOn into ChangeSettings Message
//     - Hazard: The ICD settings are set to unsafe values (i.e. to deliver
//       either a shock that is too high or too low); alternatively, the users
//       of the system (cardiologist, patient) believe that the ICD is turned on
//       (having sent the ModeOn request) when it is off.
//     - Design Item: If the ICD is off, when a ModeOn message is received from
//       an authorised principal, then the ICD should be switched to ModeOn.
//     - Keyword: OTHER THAN - the ICD does not switch to mode on, but its
//       settings are changed instead.
//
// (3) Attacker Mutates Payload of ChangeSettings Message
//     - Hazard: (As above) The ICD settings are set to unsafe values (i.e. to
//       deliver either a shock that is too high or too low)
//     - Design Item: (As in (1)) If the ICD is off, when a
//       ChangeSettings request arrived on the network from an authorised
//       principal, then the ICD should change its internal settings in
//       accordance with the message, provided that these would not set them
//       outside their safety thresholds.
//     - Keyword: MORE / LESS - the joules to deliver for ventricular
//       fibrillation are set to higher / lower than the amount specified in
//       the message.
