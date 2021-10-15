# COPYRIGHT (c) 2021 ALL RIGHT RESERVED
# Chair for Security Engineering
# Georg Land (georg.land@rub.de)
# License: see LICENSE file

# THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY 
# KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
# PARTICULAR PURPOSE.

# hacky, heuristic script that finds suitable memory mappings for the dilithium implementation

from math import ceil
import random
from copy import deepcopy
import sys


def calcpossible(mem):
  global allpoly, par
  possible = deepcopy(allpoly)
  for i,m in enumerate(mem):
    if m is None:
      break
    for r in par:
      if m+"*" in r:
        possible = [x for x in possible if x not in r and x+"*" not in r]
      if m in r:
        possible = [x for x in possible if x != m]
        for mm in mem:
          if mm is None:
            break
          if mm == m:
            continue
          if mm in r:
            possible = [x for x in possible if x not in r and x+"*" not in r]
  return possible

def getnumpoly(x):
  if type(x) in [list,tuple]:
    return sum([getnumpoly(el) for el in x])
  return 1

def ispossible(mem, poly):
  if poly in mem:
    raise ValueError
  global par
  for r in par:
    read = 0
    write = 0
    for m in mem:
      if m is None:
        continue
      if "r"+m in r:
        read += 1
      if "w"+m in r:
        write += 1
    if read > 1 or write > 1:
      raise ValueError("already impossible")
    if "r"+poly in r:
      read += 1
    if "w"+poly in r:
      write += 1
    if read > 1 or write > 1:
      return False
  return True

def find(mem, poly):
  for memory_index,m in enumerate(mem):
    for poly_index,mm in enumerate(m):
      if mm == poly:
        return (memory_index, poly_index)
  return (None, None)

for op in ["full", "keygen", "sign", "verify"]:
  print("+"+"-"*(len(op)+2)+"+")
  print(f"| {op} |")
  print("+"+"-"*(len(op)+2)+"+")
  for k,l,parmset in [(4,4,"II"),(6,5,"III"),(8,7,"V")]:

    A=[[f"A[{i},{j}]" for j in range(l)] for i in range(k)]
    s1=[f"s1[{j}]" for j in range(l)]
    s2=[f"s2[{i}]" for i in range(k)]
    t0=[f"t0[{i}]" for i in range(k)]
    t1=[f"t1[{i}]" for i in range(k)]
    w=[f"w[{i}]" for i in range(k)]
    c="c"
    zy=[f"zy[{j}]" for j in range(l)]
    tmp0=[f"tmp0[{i}]" for i in range(k)]
    tmp1=[f"tmp1[{i}]" for i in range(k)]
    
    par=[]
    
    if op in ["full", "keygen"]:
      # keygen:
      # sample s1 in s1 and t1
      for j in range(l):
        par.append(("w"+s1[j], "w"+t1[j]))
      # transform s1 (in t1) and multiply with A into t0
      for j in range(1,l):
        for i in range(k):
          if j == 1:
            par.append(("r"+t1[j], "w"+t1[j], # ntt
                        "r"+t1[j-1], "r"+A[i][j-1], "w"+t0[i])) # mul-write
          else:
            par.append(("r"+t1[j], "w"+t1[j], # ntt
                        "r"+t1[j-1], "r"+A[i][j-1], "r"+t0[i], "w"+t0[i])) # mul-read-write
      for i in range(k):
        par.append(("r"+t1[l-1], "r"+A[i][l-1], "r"+t0[i], "w"+t0[i])) # mul-read-write
      # transform t0 and add s2
      for i in range(1,k):
        par.append(("r"+t0[i], "w"+t0[i], "r"+t0[i-1], "w"+t0[i-1], "r"+s2[i-1], "w"+t1[i-1]))
      par.append(("r"+t0[k-1], "w"+t0[k-1], "r"+s2[k-1], "w"+t1[k-1]))
    
    if op in ["full", "sign"]:
      # sign:
      # transform y and multiply with A into w
      for j in range(1,l):
        for i in range(k):
          if j == 1:
            par.append(("r"+zy[j], "w"+zy[j], # ntt
                        "r"+zy[j-1], "r"+A[i][j-1], "w"+w[i])) # mul-write
          else:
            par.append(("r"+zy[j], "w"+zy[j], # ntt
                        "r"+zy[j-1], "r"+A[i][j-1], "r"+w[i], "w"+w[i])) # mul-read-write
      for i in range(k):
        par.append(("r"+zy[l-1], "r"+A[i][l-1], "r"+w[i], "w"+w[i])) # mul-read-write
      # z=y+c*s1
      for j in range(l):
        par.append(("r"+zy[j], "w"+zy[j], "r"+c, "r"+s1[j]))
      # tmp0=c*s2, then transform, then subtract from w
      par.append(("r"+c, "r"+s2[0], "w"+tmp0[0])) # mul-write
      par.append(("r"+c, "r"+s2[1], "w"+tmp0[1], # mul-write
                  "r"+tmp0[0], "w"+tmp0[0])) # ntt
      for i in range(2,k):
          par.append(("r"+c, "r"+s2[i], "w"+tmp0[i], # mul-write
                      "r"+tmp0[i-1], "w"+tmp0[i-1], # ntt
                      "r"+tmp0[i-2], "w"+tmp0[i-2], "r"+w[i-2])) # sub-read-write
      par.append(("r"+tmp0[k-1], "w"+tmp0[k-1], # ntt
                  "r"+tmp0[k-2], "w"+tmp0[k-2], "r"+w[k-2])) # sub-read-write
      par.append(("r"+tmp0[k-1], "w"+tmp0[k-1], "r"+w[k-1])) # sub-read-write
      # tmp1=c*t0, then transform
      par.append(("w"+tmp1[0], "r"+c, "r"+t0[0])) # mul-write
      for i in range(1,k):
        par.append(("r"+tmp1[i-1], "w"+tmp1[i-1], # ntt
                    "w"+tmp1[i], "r"+c, "r"+t0[i])) # mul-write
      # makehint
      for i in range(k):
        par.append(("r"+tmp0[i], "r"+tmp1[i]))
      
    if op in ["full", "verify"]:
      # verify:
      # transform z and multiply with A into w
      for j in range(1,l):
        for i in range(k):
          if j == 1:
            par.append(("r"+zy[j], "w"+zy[j], # ntt
                        "r"+zy[j-1], "r"+A[i][j-1], "w"+w[i])) # mul-write
          else:
            par.append(("r"+zy[j], "w"+zy[j], # ntt
                        "r"+zy[j-1], "r"+A[i][j-1], "r"+w[i], "w"+w[i])) # mul-read-write
      # c*t1, w-tmp0
      for i in range(k):
        par.append(("r"+c, "r"+t1[i], "w"+tmp0[i]))
        par.append(("r"+w[i], "w"+w[i], "r"+tmp0[i]))
    
    if op in ["full", "sign", "verify"]:
      for a in A:
        for aa in a:
          for ttt in t0:
            par.append(("w"+aa, "r"+ttt, "w"+ttt))
          for ttt in s1:
            par.append(("w"+aa, "r"+ttt, "w"+ttt))
          for ttt in s2:
            par.append(("w"+aa, "r"+ttt, "w"+ttt))
          for ttt in t1:
            par.append(("w"+aa, "r"+ttt, "w"+ttt))

    allpoly = []
    for i in range(k):
      allpoly.extend(A[i])
    if op in ["full", "sign", "keygen"]:
      allpoly.extend(s1)
      allpoly.extend(s2)
      allpoly.extend(t0)
    if op in ["full", "sign"]:
      allpoly.extend(tmp1)
    allpoly.extend(t1)
    allpoly.extend(w)
    if op in ["full", "sign", "verify"]:
      allpoly.extend(tmp0)
      allpoly.extend([c])
    if op != "keygen":
      allpoly.extend(zy)
    print(op, allpoly)
    
    optimal = ceil(getnumpoly(allpoly)/8.0)
    print((k,l,optimal))
    
    minmemlen = 10000
    minmem = None
    minmatch = 10000
    try:
      for trycount in range(100000):
        #memory = [[]]
        #for aas in A:
          #for aaas in aas:
            #memory[-1].append(aaas)
            #if len(memory[-1]) == 8:
              #memory.append([])
        #while len(memory[-1]) != 8:
          #memory[-1].append(None)
        startpoly = allpoly[random.randint(0,len(allpoly)-1)]
        memory = [[None]*8]
        memory[0][0] = startpoly
        remaining = [x for x in allpoly if x != startpoly]#deepcopy(allpoly)
        while len(remaining) > 0:
          poly = remaining.pop(random.randint(0,len(remaining)-1))
          found = False
          for mem in memory:
            if None in mem: # there are free places
              if ispossible(mem, poly):
                index = mem.index(None)
                mem[index] = poly
                found = True
                break
          if not found:
            memory.append([poly]+[None]*7)
        if len(memory) < minmemlen:
          minmem = deepcopy(memory)
          minmemlen=len(memory)
          print(minmemlen)
          if minmemlen == optimal:
            print("yeehaw")
            raise KeyboardInterrupt
          if minmemlen < optimal:
            raise ValueError("oops, there is something wrong here!")
        elif len(memory) == minmemlen:
          free=0
          for mem in memory:
            for m in mem:
              if m is None:
                free += 1
          if free < minmatch:
            minmatch = free
            print("match: ", free, trycount)
    except KeyboardInterrupt:
      pass
    
    with open(f"{parmset}_{op}/memmap.vhd", "w") as f:
      print(f"writing to {parmset}_{op}/memmap.vhd")
      tmpstdout = sys.stdout
      sys.stdout = f
      
      print(f"""
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;

package memmap is

constant NUM_MEM_8_POLY : natural := {minmemlen};

type map_type is record
    memory_index : natural range 0 to NUM_MEM_8_POLY;
    poly_index   : natural range 0 to 8;
end record map_type;

constant INVALID_MAP : map_type := (memory_index => NUM_MEM_8_POLY, poly_index => 8);

type mat_map_type is array(0 to {k-1}, 0 to {l-1}) of map_type;
type vecl_map_type is array(0 to {l-1}) of map_type;
type veck_map_type is array(0 to {k-1}) of map_type;

type memory_map_type is record
    A : mat_map_type;
    s1 : vecl_map_type;
    s2 : veck_map_type;
    t0 : veck_map_type;
    t1 : veck_map_type;
    zy : vecl_map_type;
    w  : veck_map_type;
    c  : map_type;
    tmp0 : veck_map_type;
    tmp1 : veck_map_type;
end record memory_map_type;
    
type matvecaddr_type is (none, A, s1, s2, t0, t1, y, w, c, z, tmp0, tmp1);

constant memory_map : memory_map_type := (
""")
      
      
      print("A=>(",end="")
      for i in range(k):
        print(f"{i}=>(", end="")
        for j in range(l):
          mi, pi = find(minmem, A[i][j])
          print(f"{j}=>(memory_index => {mi},poly_index => {pi})", end=("," if j<l-1 else ""))
        print(")", end=("," if i < k-1 else ""))
      print("),")
      # vecl
      for vecl in [s1,zy]:
        if vecl[0] in allpoly:
          print(f"{vecl[0][:-3]}=>(", end="")
          for j in range(l):
            mi,pi=find(minmem, vecl[j])
            print(f"{j}=>(memory_index=>{mi},poly_index=>{pi})", end=("," if j<l-1 else ""))
          print("),")
        else:
          tmpstdout.write(f"{vecl[0]} not in allpoly\n")
          print(f"{vecl[0][:-3]}=>(others=>INVALID_MAP),")
      # veck
      for veck in [s2,t0,t1,w,tmp0,tmp1]:
        if veck[0] in allpoly:
          print(f"{veck[0][:-3]}=>(", end="")
          for i in range(k):
            mi,pi=find(minmem, veck[i])
            print(f"{i}=>(memory_index=>{mi},poly_index=>{pi})", end=("," if i<k-1 else ""))
          print("),")
        else:
          tmpstdout.write(f"{veck[0]} not in allpoly\n")
          print(f"{veck[0][:-3]}=>(others=>INVALID_MAP),")
      # c
      if c in allpoly:
        mi,pi = find(minmem, c)
        print(f"""c=>(memory_index => {mi},poly_index => {pi})""")
      else:
        tmpstdout.write(f"{c} not in allpoly\n")
        print("c=>INVALID_MAP")
      print("""
);

end memmap;""")
      sys.stdout = tmpstdout  

    for mem in minmem:
      for m in mem:
        if m is None:
          m = ""
        print("{:8s}".format(m), end="")
      print()
    print("\n\n\n\n")

