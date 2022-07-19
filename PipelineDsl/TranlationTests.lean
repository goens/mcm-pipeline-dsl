import PipelineDsl.AST
import PipelineDsl.Murphi

open Pipeline
open Murϕ
/-
transition await_translation {
phys_addr = virt_addr
transition await_fwd_check
}
-/

def assignmentTest := Statement.variable_assignment (QualifiedName.mk ["phys_addr"]) (Expr.some_term $ Term.var "virt_addr")
def transitionStatementTest := Statement.transition "await_fwd_check"
def awaitTransitionTest := Description.transition "await_translation" $ Statement.block [assignmentTest, transitionStatementTest]

-->

def testOutput := [murϕ|
ruleset j : cores_t do
ruleset i : LD_ENTRY do
rule "await_translation TO await_fwd_check"
  Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_translation
==>
-- decls
var next_state : STATE;
var ld_entry : LD_ENTRY_VALUES;
begin
next_state := Sta;
ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];

--# "translate" the address
--#NOTE TODO: access tlb? not quite necessary for litmus tests
ld_entry.phys_addr := ld_entry.virt_addr;

ld_entry.ld_state := await_fwd_check;

next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;

Sta := next_state
end;
endruleset;
endruleset
]

def testFun : Description → Rule
  | .transition name stmt =>
    [murϕ|
    ruleset j : cores_t do
    ruleset i : LD_ENTRY do
    rule £name
    Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_translation
    ==>
    -- decls
    var next_state : STATE;
    var ld_entry : LD_ENTRY_VALUES;
    begin
    next_state := Sta;
    ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
    ld_entry.phys_addr := ld_entry.virt_addr;
    ld_entry.ld_state := await_fwd_check;
    next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;
    Sta := next_state
    end;
    endruleset;
    endruleset
    ]
  | _ => [murϕ| rule "unimplemented" true ==> begin Sta := Sta end]
