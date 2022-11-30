This file was generated from `grep '/\* Opcode:' -A3 sqlite/src/vdbe.c`

- ✅ − 18
- ❌ − 167

---

### ✅ `Goto * P2 * * *`
An unconditional jump to address P2.

### ❌ `Gosub P1 P2 * * *`
Write the current address onto register P1 and then jump to address P2.

### ❌ `Return P1 * P3 * *`
Jump to the next instruction after the address in register P1.

### ❌ `InitCoroutine P1 P2 P3 * *`
Set up register P1 so that it will Yield to the coroutine located
at address P3.

### ❌ `EndCoroutine P1 * * * *`
The instruction at the address in register P1 is a Yield. Jump to the P2
parameter of that Yield.

### ❌ `Yield P1 P2 * * *`
Swap the program counter with the value in register P1.  This has the effect
of yielding to a coroutine.

### ❌ `HaltIfNull  P1 P2 P3 P4 P5`
Synopsis: `if r[P3]=null halt`

Check the value in register P3.  If it is NULL then Halt using...

### ✅ `Halt P1 P2 * P4 P5`
Exit immediately.  All open cursors, etc are closed automatically.

### ❌ `BeginSubrtn P1 P2 * * *`
Synopsis: `r[P2]=P1`

Mark the beginning of a subroutine by loading the integer value P1

### ✅ `Integer P1 P2 * * *`
Synopsis: `r[P2]=P1`

The 32-bit integer value P1 is written into register P2.

### ❌ `Int64 * P2 * P4 *`
Synopsis: `r[P2]=P4`

P4 is a pointer to a 64-bit integer value.

### ✅ `Real * P2 * P4 *`
Synopsis: `r[P2]=P4`

P4 is a pointer to a 64-bit floating point value.

### ✅ `String8 * P2 * P4 *`
Synopsis: `r[P2]='P4'`

P4 points to a nul terminated UTF-8 string. This opcode is transformed 

### ✅ `String P1 P2 P3 P4 P5`
Synopsis: `r[P2]='P4' (len=P1)`

The string value P4 of length P1 (bytes) is stored in register P2.

### ❌ `Null P1 P2 P3 * *`
Synopsis: `r[P2..P3]=NULL`

Write a NULL into registers P2.  If P3 greater than P2, then also write

### ❌ `SoftNull P1 * * * *`
Synopsis: `r[P1]=NULL`

Set register P1 to have the value NULL as seen by the OP_MakeRecord

### ❌ `Blob P1 P2 * P4 *`
Synopsis: `r[P2]=P4 (len=P1)`

P4 points to a blob of data P1 bytes long.  Store this

### ❌ `Variable P1 P2 * P4 *`
Synopsis: `r[P2]=parameter(P1,P4)`

Transfer the values of bound parameter P1 into register P2

### ❌ `Move P1 P2 P3 * *`
Synopsis: `r[P2@P3]=r[P1@P3]`

Move the P3 values in register P1..P1+P3-1 over into

### ❌ `Copy P1 P2 P3 * *`
Synopsis: `r[P2@P3+1]=r[P1@P3+1]`

Make a copy of registers P1..P1+P3 into registers P2..P2+P3.

### ❌ `SCopy P1 P2 * * *`
Synopsis: `r[P2]=r[P1]`

Make a shallow copy of register P1 into register P2.

### ❌ `IntCopy P1 P2 * * *`
Synopsis: `r[P2]=r[P1]`

Transfer the integer value held in register P1 into register P2.

### ❌ `FkCheck * * * * *`

Halt with an SQLITE_CONSTRAINT error if there are any unresolved
foreign key constraint violations.  If there are no foreign key

### ✅ `ResultRow P1 P2 * * *`
Synopsis: `output=r[P1@P2]`

The registers P1 through P1+P2-1 contain a single row of

### ❌ `Concat P1 P2 P3 * *`
Synopsis: `r[P3]=r[P2]+r[P1]`

Add the text in register P1 onto the end of the text in

### ❌ `Add P1 P2 P3 * *`
Synopsis: `r[P3]=r[P1]+r[P2]`

Add the value in register P1 to the value in register P2

### ❌ `Multiply P1 P2 P3 * *`
Synopsis: `r[P3]=r[P1]*r[P2]`

### ❌ `Subtract P1 P2 P3 * *`
Synopsis: `r[P3]=r[P2]-r[P1]`

Subtract the value in register P1 from the value in register P2

### ❌ `Divide P1 P2 P3 * *`
Synopsis: `r[P3]=r[P2]/r[P1]`

Divide the value in register P1 by the value in register P2

### ❌ `Remainder P1 P2 P3 * *`
Synopsis: `r[P3]=r[P2]%r[P1]`

Compute the remainder after integer register P2 is divided by 

### ❌ `CollSeq P1 * * P4`
P4 is a pointer to a CollSeq object. If the next call to a user function
or aggregate calls sqlite3GetFuncCollSeq(), this collation sequence will

### ❌ `BitAnd P1 P2 P3 * *`
Synopsis: `r[P3]=r[P1]&r[P2]`

Take the bit-wise AND of the values in register P1 and P2 and

### ❌ `BitOr P1 P2 P3 * *`
Synopsis: `r[P3]=r[P1]|r[P2]`

Take the bit-wise OR of the values in register P1 and P2 and

### ❌ `ShiftLeft P1 P2 P3 * *`
Synopsis: `r[P3]=r[P2]<<r[P1]`

Shift the integer value in register P2 to the left by the

### ❌ `ShiftRight P1 P2 P3 * *`
Synopsis: `r[P3]=r[P2]>>r[P1]`

Shift the integer value in register P2 to the right by the

### ❌ `AddImm  P1 P2 * * *`
Synopsis: `r[P1]=r[P1]+P2`

Add the constant P2 to the value in register P1.

### ❌ `MustBeInt P1 P2 * * *`

Force the value in register P1 to be an integer.  If the value
in P1 is not an integer and cannot be converted into an integer

### ❌ `RealAffinity P1 * * * *`

If register P1 holds an integer convert it to a real value.

### ❌ `Cast P1 P2 * * *`
Synopsis: `affinity(r[P1])`

Force the value in register P1 to be the type defined by P2.

### ✅ `Eq P1 P2 P3 P4 P5`
Synopsis: `IF r[P3]==r[P1]`

Compare the values in register P1 and P3.  If reg(P3)==reg(P1) then

### ✅ `Ne P1 P2 P3 P4 P5`
Synopsis: `IF r[P3]!=r[P1]`

### ✅ `Lt P1 P2 P3 P4 P5`
Synopsis: `IF r[P3]<r[P1]`

Compare the values in register P1 and P3.

### ✅ `Le P1 P2 P3 P4 P5`
Synopsis: `IF r[P3]<=r[P1]`

### ✅ `Gt P1 P2 P3 P4 P5`
Synopsis: `IF r[P3]>r[P1]`

### ✅ `Ge P1 P2 P3 P4 P5`
Synopsis: `IF r[P3]>=r[P1]`

### ❌ `ElseEq * P2 * * *`

This opcode must follow an OP_Lt or OP_Gt comparison operator.  There
can be zero or more OP_ReleaseReg opcodes intervening, but no other

### ❌ `Permutation * * * P4 *`

Set the permutation used by the OP_Compare operator in the next
instruction.  The permutation is stored in the P4 operand.

### ❌ `Compare P1 P2 P3 P4 P5`
Synopsis: `r[P1@P3] <-> r[P2@P3]`

Compare two vectors of registers in reg(P1)..reg(P1+P3-1) (call this

### ❌ `Jump P1 P2 P3 * *`

Jump to the instruction at address P1, P2, or P3 depending on whether
in the most recent OP_Compare instruction the P1 vector was less than

### ❌ `And P1 P2 P3 * *`
Synopsis: `r[P3]=(r[P1] && r[P2])`

### ❌ `Or P1 P2 P3 * *`
Synopsis: `r[P3]=(r[P1] || r[P2])`

### ❌ `IsTrue P1 P2 P3 P4 *`
Synopsis: `r[P2] = coalesce(r[P1]==TRUE,P3) ^ P4`

This opcode implements the IS TRUE, IS FALSE, IS NOT TRUE, and

### ❌ `Not P1 P2 * * *`
Synopsis: `r[P2]= !r[P1]`

Interpret the value in register P1 as a boolean value.  Store the

### ❌ `BitNot P1 P2 * * *`
Synopsis: `r[P2]= ~r[P1]`

Interpret the content of register P1 as an integer.  Store the

### ❌ `Once P1 P2 * * *`
Fall through to the next instruction the first time this opcode is
encountered on each invocation of the byte-code program.  Jump to P2

### ❌ `If P1 P2 P3 * *`
Jump to P2 if the value in register P1 is true.  The value
is considered true if it is numeric and non-zero.  If the value

### ❌ `IfNot P1 P2 P3 * *`
Jump to P2 if the value in register P1 is False.  The value
is considered false if it has a numeric value of zero.  If the value

### ❌ `IsNull P1 P2 * * *`
Synopsis: `if r[P1]==NULL goto P2`

Jump to P2 if the value in register P1 is NULL.

### ❌ `IsNullOrType P1 P2 P3 * *`
Synopsis: `if typeof(r[P1]) IN (P3,5) goto P2`

Jump to P2 if the value in register P1 is NULL or has a datatype P3.

### ❌ `ZeroOrNull P1 P2 P3 * *`
Synopsis: `r[P2] = 0 OR NULL`

If all both registers P1 and P3 are NOT NULL, then store a zero in

### ❌ `NotNull P1 P2 * * *`
Synopsis: `if r[P1]!=NULL goto P2`

Jump to P2 if the value in register P1 is not NULL.  

### ❌ `IfNullRow P1 P2 P3 * *`
Synopsis: `if P1.nullRow then r[P3]=NULL, goto P2`

Check the cursor P1 to see if it is currently pointing at a NULL row.

### ❌ `Offset P1 P2 P3 * *`
Synopsis: `r[P3] = sqlite_offset(P1)`

Store in register r[P3] the byte offset into the database file that is the

### ✅ `Column P1 P2 P3 P4 P5`
Synopsis: `r[P3]=PX`

Interpret the data that cursor P1 points to as a structure built using

### ❌ `TypeCheck P1 P2 P3 P4 *`
Synopsis: `typecheck(r[P1@P2])`

Apply affinities to the range of P2 registers beginning with P1.

### ❌ `Affinity P1 P2 * P4 *`
Synopsis: `affinity(r[P1@P2])`

Apply affinities to a range of P2 registers starting with P1.

### ❌ `MakeRecord P1 P2 P3 P4 *`
Synopsis: `r[P3]=mkrec(r[P1@P2])`

Convert P2 registers beginning with P1 into the [record format]

### ❌ `Count P1 P2 P3 * *`
Synopsis: `r[P2]=count()`

Store the number of entries (an integer value) in the table or index 

### ❌ `Savepoint P1 * * P4 *`

Open, release or rollback the savepoint named by parameter P4, depending
on the value of P1. To open a new savepoint set P1==0 (SAVEPOINT_BEGIN).

### ❌ `AutoCommit P1 P2 * * *`

Set the database auto-commit flag to P1 (1 or 0). If P2 is true, roll
back any currently active btree transactions. If there are any active

### ❌ `Transaction P1 P2 P3 P4 P5`

Begin a transaction on database P1 if a transaction is not already
active.

### ❌ `ReadCookie P1 P2 P3 * *`

Read cookie number P3 from database P1 and write it into register P2.
P3==1 is the schema version.  P3==2 is the database format.

### ❌ `SetCookie P1 P2 P3 * P5`

Write the integer value P3 into cookie number P2 of database P1.
P2==1 is the schema version.  P2==2 is the database format.

### ✅ `OpenRead P1 P2 P3 P4 P5`
Synopsis: `root=P2 iDb=P3`

Open a read-only cursor for the database table whose root page is

### ❌ `ReopenIdx P1 P2 P3 P4 P5`
Synopsis: `root=P2 iDb=P3`

The ReopenIdx opcode works like OP_OpenRead except that it first

### ❌ `OpenWrite P1 P2 P3 P4 P5`
Synopsis: `root=P2 iDb=P3`

Open a read/write cursor named P1 on the table or index whose root

### ❌ `OpenDup P1 P2 * * *`

Open a new cursor P1 that points to the same ephemeral table as
cursor P2.  The P2 cursor must have been opened by a prior OP_OpenEphemeral

### ❌ `OpenEphemeral P1 P2 P3 P4 P5`
Synopsis: `nColumn=P2`

Open a new cursor P1 to a transient table.

### ❌ `OpenAutoindex P1 P2 * P4 *`
Synopsis: `nColumn=P2`

This opcode works the same as OP_OpenEphemeral.  It has a

### ❌ `SorterOpen P1 P2 P3 P4 *`

This opcode works like OP_OpenEphemeral except that it opens
a transient index that is specifically designed to sort large

### ❌ `SequenceTest P1 P2 * * *`
Synopsis: `if( cursor[P1].ctr++ ) pc = P2`

P1 is a sorter cursor. If the sequence counter is currently zero, jump

### ❌ `OpenPseudo P1 P2 P3 * *`
Synopsis: `P3 columns in r[P2]`

Open a new cursor that points to a fake table that contains a single

### ❌ `Close P1 * * * *`

Close a cursor previously opened as P1.  If P1 is not
currently open, this instruction is a no-op.

### ❌ `ColumnsUsed P1 * * P4 *`

This opcode (which only exists if SQLite was compiled with
SQLITE_ENABLE_COLUMN_USED_MASK) identifies which columns of the

### ❌ `SeekGE P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

If cursor P1 refers to an SQL table (B-Tree that uses integer keys), 

### ❌ `SeekGT P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

If cursor P1 refers to an SQL table (B-Tree that uses integer keys), 

### ❌ `SeekLT P1 P2 P3 P4 * `
Synopsis: `key=r[P3@P4]`

If cursor P1 refers to an SQL table (B-Tree that uses integer keys), 

### ❌ `SeekLE P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

If cursor P1 refers to an SQL table (B-Tree that uses integer keys), 

### ❌ `SeekScan  P1 P2 * * *`
Synopsis: `Scan-ahead up to P1 rows`

This opcode is a prefix opcode to OP_SeekGE.  In other words, this

### ❌ `SeekHit P1 P2 P3 * *`
Synopsis: `set P2<=seekHit<=P3`

Increase or decrease the seekHit value for cursor P1, if necessary,

### ❌ `IfNotOpen P1 P2 * * *`
Synopsis: `if( !csr[P1] ) goto P2`

If cursor P1 is not open, jump to instruction P2. Otherwise, fall through.

### ❌ `Found P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

If P4==0 then register P3 holds a blob constructed by MakeRecord.  If

### ❌ `NotFound P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

If P4==0 then register P3 holds a blob constructed by MakeRecord.  If

### ❌ `IfNoHope P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

Register P3 is the first of P4 registers that form an unpacked

### ❌ `NoConflict P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

If P4==0 then register P3 holds a blob constructed by MakeRecord.  If

### ❌ `SeekRowid P1 P2 P3 * *`
Synopsis: `intkey=r[P3]`

P1 is the index of a cursor open on an SQL table btree (with integer

### ❌ `NotExists P1 P2 P3 * *`
Synopsis: `intkey=r[P3]`

P1 is the index of a cursor open on an SQL table btree (with integer

### ❌ `Sequence P1 P2 * * *`
Synopsis: `r[P2]=cursor[P1].ctr++`

Find the next available sequence number for cursor P1.

### ❌ `NewRowid P1 P2 P3 * *`
Synopsis: `r[P2]=rowid`

Get a new integer record number (a.k.a "rowid") used as the key to a table.

### ❌ `Insert P1 P2 P3 P4 P5`
Synopsis: `intkey=r[P3] data=r[P2]`

Write an entry into the table of cursor P1.  A new entry is

### ❌ `RowCell P1 P2 P3 * *`

P1 and P2 are both open cursors. Both must be opened on the same type
of table - intkey or index. This opcode is used as part of copying

### ❌ `Delete P1 P2 P3 P4 P5`

Delete the record at which the P1 cursor is currently pointing.


### ❌ `ResetCount * * * * *`

The value of the change counter is copied to the database handle
change counter (returned by subsequent calls to sqlite3_changes()).

### ❌ `SorterCompare P1 P2 P3 P4`
Synopsis: `if key(P1)!=trim(r[P3],P4) goto P2`

P1 is a sorter cursor. This instruction compares a prefix of the

### ❌ `SorterData P1 P2 P3 * *`
Synopsis: `r[P2]=data`

Write into register P2 the current sorter data for sorter cursor P1.

### ❌ `RowData P1 P2 P3 * *`
Synopsis: `r[P2]=data`

Write into register P2 the complete row content for the row at 

### ❌ `Rowid P1 P2 * * *`
Synopsis: `r[P2]=rowid`

Store in register P2 an integer which is the key of the table entry that

### ❌ `NullRow P1 * * * *`

Move the cursor P1 to a null row.  Any OP_Column operations
that occur while the cursor is on the null row will always

### ❌ `SeekEnd P1 * * * *`

Position cursor P1 at the end of the btree for the purpose of
appending a new entry onto the btree.

### ❌ `Last P1 P2 * * *`

The next use of the Rowid or Column or Prev instruction for P1 
will refer to the last entry in the database table or index.

### ❌ `IfSmaller P1 P2 P3 * *`

Estimate the number of rows in the table P1.  Jump to P2 if that
estimate is less than approximately 2**(0.1*P3).

### ❌ `SorterSort P1 P2 * * *`

After all records have been inserted into the Sorter object
identified by P1, invoke this opcode to actually do the sorting.

### ❌ `Sort P1 P2 * * *`

This opcode does exactly the same thing as OP_Rewind except that
it increments an undocumented global variable used for testing.

### ✅ `Rewind P1 P2 * * *`

The next use of the Rowid or Column or Next instruction for P1 
will refer to the first entry in the database table or index.

### ✅ `Next P1 P2 P3 * P5`

Advance cursor P1 so that it points to the next key/data pair in its
table or index.  If there are no more key/value pairs then fall through

### ❌ `Prev P1 P2 P3 * P5`

Back up cursor P1 so that it points to the previous key/data pair in its
table or index.  If there is no previous key/value pairs then fall through

### ❌ `SorterNext P1 P2 * * P5`

This opcode works just like OP_Next except that P1 must be a
sorter object for which the OP_SorterSort opcode has been

### ❌ `IdxInsert P1 P2 P3 P4 P5`
Synopsis: `key=r[P2]`

Register P2 holds an SQL index key made using the

### ❌ `SorterInsert P1 P2 * * *`
Synopsis: `key=r[P2]`

Register P2 holds an SQL index key made using the

### ❌ `IdxDelete P1 P2 P3 * P5`
Synopsis: `key=r[P2@P3]`

The content of P3 registers starting at register P2 form

### ❌ `DeferredSeek P1 * P3 P4 *`
Synopsis: `Move P3 to P1.rowid if needed`

P1 is an open index cursor and P3 is a cursor on the corresponding

### ❌ `IdxRowid P1 P2 * * *`
Synopsis: `r[P2]=rowid`

Write into register P2 an integer which is the last entry in the record at

### ❌ `FinishSeek P1 * * * *`

If cursor P1 was previously moved via OP_DeferredSeek, complete that
seek operation now, without further delay.  If the cursor seek has

### ❌ `IdxGE P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

The P4 register values beginning with P3 form an unpacked index 

### ❌ `IdxGT P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

The P4 register values beginning with P3 form an unpacked index 

### ❌ `IdxLT P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

The P4 register values beginning with P3 form an unpacked index 

### ❌ `IdxLE P1 P2 P3 P4 *`
Synopsis: `key=r[P3@P4]`

The P4 register values beginning with P3 form an unpacked index 

### ❌ `Destroy P1 P2 P3 * *`

Delete an entire database table or index whose root page in the database
file is given by P1.

### ❌ `Clear P1 P2 P3`

Delete all contents of the database table or index whose root page
in the database file is given by P1.  But, unlike Destroy, do not

### ❌ `ResetSorter P1 * * * *`

Delete all contents from the ephemeral table or sorter
that is open on cursor P1.

### ❌ `CreateBtree P1 P2 P3 * *`
Synopsis: `r[P2]=root iDb=P1 flags=P3`

Allocate a new b-tree in the main database file if P1==0 or in the

### ❌ `SqlExec * * * P4 *`

Run the SQL statement or statements specified in the P4 string.

### ❌ `ParseSchema P1 * * P4 *`

Read and parse all entries from the schema table of database P1
that match the WHERE clause P4.  If P4 is a NULL pointer, then the

### ❌ `LoadAnalysis P1 * * * *`

Read the sqlite_stat1 table for database P1 and load the content
of that table into the internal index hash table.  This will cause

### ❌ `DropTable P1 * * P4 *`

Remove the internal (in-memory) data structures that describe
the table named P4 in database P1.  This is called after a table

### ❌ `DropIndex P1 * * P4 *`

Remove the internal (in-memory) data structures that describe
the index named P4 in database P1.  This is called after an index

### ❌ `DropTrigger P1 * * P4 *`

Remove the internal (in-memory) data structures that describe
the trigger named P4 in database P1.  This is called after a trigger

### ❌ `IntegrityCk P1 P2 P3 P4 P5`

Do an analysis of the currently open database.  Store in
register P1 the text of an error message describing any problems.

### ❌ `RowSetAdd P1 P2 * * *`
Synopsis: `rowset(P1)=r[P2]`

Insert the integer value held by register P2 into a RowSet object

### ❌ `RowSetRead P1 P2 P3 * *`
Synopsis: `r[P3]=rowset(P1)`

Extract the smallest value from the RowSet object in P1

### ❌ `RowSetTest P1 P2 P3 P4`
Synopsis: `if r[P3] in rowset(P1) goto P2`

Register P3 is assumed to hold a 64-bit integer value. If register P1

### ❌ `Program P1 P2 P3 P4 P5`

Execute the trigger program passed as P4 (type P4_SUBPROGRAM). 


### ❌ `Param P1 P2 * * *`

This opcode is only ever present in sub-programs called via the 
OP_Program instruction. Copy a value currently stored in a memory 

### ❌ `FkCounter P1 P2 * * *`
Synopsis: `fkctr[P1]+=P2`

Increment a "constraint counter" by P2 (P2 may be negative or positive).

### ❌ `FkIfZero P1 P2 * * *`
Synopsis: `if fkctr[P1]==0 goto P2`

This opcode tests if a foreign key constraint-counter is currently zero.

### ❌ `MemMax P1 P2 * * *`
Synopsis: `r[P1]=max(r[P1],r[P2])`

P1 is a register in the root frame of this VM (the root frame is

### ❌ `IfPos P1 P2 P3 * *`
Synopsis: `if r[P1]>0 then r[P1]-=P3, goto P2`

Register P1 must contain an integer.

### ❌ `OffsetLimit P1 P2 P3 * *`
Synopsis: `if r[P1]>0 then r[P2]=r[P1]+max(0,r[P3]) else r[P2]=(-1)`

This opcode performs a commonly used computation associated with

### ❌ `IfNotZero P1 P2 * * *`
Synopsis: `if r[P1]!=0 then r[P1]--, goto P2`

Register P1 must contain an integer.  If the content of register P1 is

### ❌ `DecrJumpZero P1 P2 * * *`
Synopsis: `if (--r[P1])==0 goto P2`

Register P1 must hold an integer.  Decrement the value in P1

### ❌ `AggStep * P2 P3 P4 P5`
Synopsis: `accum=r[P3] step(r[P2@P5])`

Execute the xStep function for an aggregate.

### ❌ `AggInverse * P2 P3 P4 P5`
Synopsis: `accum=r[P3] inverse(r[P2@P5])`

Execute the xInverse function for an aggregate.

### ❌ `AggStep1 P1 P2 P3 P4 P5`
Synopsis: `accum=r[P3] step(r[P2@P5])`

Execute the xStep (if P1==0) or xInverse (if P1!=0) function for an

### ❌ `AggFinal P1 P2 * P4 *`
Synopsis: `accum=r[P1] N=P2`

P1 is the memory location that is the accumulator for an aggregate

### ❌ `AggValue * P2 P3 P4 *`
Synopsis: `r[P3]=value N=P2`

Invoke the xValue() function and store the result in register P3.

### ❌ `Checkpoint P1 P2 P3 * *`

Checkpoint database P1. This is a no-op if P1 is not currently in
WAL mode. Parameter P2 is one of SQLITE_CHECKPOINT_PASSIVE, FULL,

### ❌ `JournalMode P1 P2 P3 * *`

Change the journal mode of database P1 to P3. P3 must be one of the
PAGER_JOURNALMODE_XXX values. If changing between the various rollback

### ❌ `Vacuum P1 P2 * * *`

Vacuum the entire database P1.  P1 is 0 for "main", and 2 or more
for an attached database.  The "temp" database may not be vacuumed.

### ❌ `IncrVacuum P1 P2 * * *`

Perform a single step of the incremental vacuum procedure on
the P1 database. If the vacuum has finished, jump to instruction

### ❌ `Expire P1 P2 * * *`

Cause precompiled statements to expire.  When an expired statement
is executed using sqlite3_step() it will either automatically

### ❌ `CursorLock P1 * * * *`

Lock the btree to which cursor P1 is pointing so that the btree cannot be
written by an other cursor.

### ❌ `CursorUnlock P1 * * * *`

Unlock the btree to which cursor P1 is pointing so that it can be
written by other cursors.

### ❌ `TableLock P1 P2 P3 P4 *`
Synopsis: `iDb=P1 root=P2 write=P3`

Obtain a lock on a particular table. This instruction is only used when

### ❌ `VBegin * * * P4 *`

P4 may be a pointer to an sqlite3_vtab structure. If so, call the 
xBegin method for that table.

### ❌ `VCreate P1 P2 * * *`

P2 is a register that holds the name of a virtual table in database 
P1. Call the xCreate method for that table.

### ❌ `VDestroy P1 * * P4 *`

P4 is the name of a virtual table in database P1.  Call the xDestroy method
of that table.

### ❌ `VOpen P1 * * P4 *`

P4 is a pointer to a virtual table object, an sqlite3_vtab structure.
P1 is a cursor number.  This opcode opens a cursor to the virtual

### ❌ `VInitIn P1 P2 P3 * *`
Synopsis: `r[P2]=ValueList(P1,P3)`

Set register P2 to be a pointer to a ValueList object for cursor P1

### ❌ `VFilter P1 P2 P3 P4 *`
Synopsis: `iplan=r[P3] zplan='P4'`

P1 is a cursor opened using VOpen.  P2 is an address to jump to if

### ❌ `VColumn P1 P2 P3 * P5`
Synopsis: `r[P3]=vcolumn(P2)`

Store in register P3 the value of the P2-th column of

### ❌ `VNext P1 P2 * * *`

Advance virtual table P1 to the next row in its result set and
jump to instruction P2.  Or, if the virtual table has reached

### ❌ `VRename P1 * * P4 *`

P4 is a pointer to a virtual table object, an sqlite3_vtab structure.
This opcode invokes the corresponding xRename method. The value

### ❌ `VUpdate P1 P2 P3 P4 P5`
Synopsis: `data=r[P3@P2]`

P4 is a pointer to a virtual table object, an sqlite3_vtab structure.

### ❌ `Pagecount P1 P2 * * *`

Write the current number of pages in database P1 to memory cell P2.
*/

### ❌ `MaxPgcnt P1 P2 P3 * *`

Try to set the maximum page count for database P1 to the value in P3.
Do not let the maximum page count fall below the current page count and

### ❌ `Function P1 P2 P3 P4 *`
Synopsis: `r[P3]=func(r[P2@NP])`

Invoke a user function (P4 is a pointer to an sqlite3_context object that

### ❌ `PureFunc P1 P2 P3 P4 *`
Synopsis: `r[P3]=func(r[P2@NP])`

Invoke a user function (P4 is a pointer to an sqlite3_context object that

### ❌ `FilterAdd P1 * P3 P4 *`
Synopsis: `filter(P1) += key(P3@P4)`

Compute a hash on the P4 registers starting with r[P3] and

### ❌ `Filter P1 P2 P3 P4 *`
Synopsis: `if key(P3@P4) not in filter(P1) goto P2`

Compute a hash on the key contained in the P4 registers starting

### ❌ `Trace P1 P2 * P4 *`

Write P4 on the statement trace output if statement tracing is
enabled.

### ✅ `Init P1 P2 P3 P4 *`
Synopsis: `Start at P2`

Programs contain a single instance of this opcode as the very first

### ❌ `CursorHint P1 * * P4 *`

Provide a hint to cursor P1 that it only needs to return rows that
satisfy the Expr in P4.  TK_REGISTER terms in the P4 expression refer

### ❌ `Abortable   * * * * *`

Verify that an Abort can happen.  Assert if an Abort at this point
might cause database corruption.  This opcode only appears in debugging

### ❌ `ReleaseReg   P1 P2 P3 * P5`
Synopsis: `release r[P1@P2] mask P3`

Release registers from service.  Any content that was in the

### ❌ `Noop * * * * *`

Do nothing.  This instruction is often useful as a jump
destination.
