
inductive LSQ
| HP : LSQ
| LB : LSQ
| Unified : LSQ

def String.toLSQ
| lsq_type =>
  if lsq_type == "HP" then
    some LSQ.HP
  else if lsq_type == "LB" then
    some LSQ.LB
  else if lsq_type == "Unified" then
    some LSQ.Unified
  else
    none

inductive TFSM
| IO : TFSM
| LR : TFSM
| IT : TFSM

def String.toTFSM
| tfsm_selected =>
  if tfsm_selected == "IO" then
    some TFSM.IO
  else if tfsm_selected== "LR" then
    some TFSM.LR
  else if tfsm_selected== "IT" then
    some TFSM.IT
  else
    none
