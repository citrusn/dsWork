| package |
package := Package name: 'fat'.
package paxVersion: 1;
	basicComment: 'BIOS parameter block -> bpbSecPerClust = 1 and bpbBytesPerSec=512 !!!!
0: 
	boot sector + reserved sectors =  BIOS parameter block -> bpbResSectors
BIOS parameter block ->reserved sectors :
	fat 1 = BIOS parameter block -> bpbFATsecs 
	fat 2 = BIOS parameter block -> bpbFATsecs
+ BIOS parameter block -> bpbFATs * BIOS parameter block -> bpbFATsecs: 
	root directory = BIOS parameter block ->  bpbRootDirEnts * sizeof(struct direntry)
+ BIOS parameter block ->  bpbRootDirEnts * sizeof(struct direntry) :
	data area'.


package classNames
	add: #BootSector33;
	add: #BPB33;
	add: #Fat;
	add: #FatDirEntry;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Fat
	instanceVariableNames: 'buffer btSector bpb'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #BootSector33
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #BPB33
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FatDirEntry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Fat guid: (GUID fromString: '{fa205312-066a-4f3c-b9c5-073305477e0a}')!
Fat comment: 'buffer - place for storaging image file
btSector - bootSector
bpb- Bios Parameters Block'!
!Fat categoriesForClass!Kernel-Objects! !
!Fat methodsFor!

bpb
	^bpb!

btSector
	^btSector!

buffer
	^buffer!

findFile: anFile cluster:anCluster  mode:aMode  
	| entry offset seekName nextName|
	"entry := FatDirEntry fromBytes: (buffer copyStringFrom: offset to: offset + FatDirEntry byteSize)."
	seekName := anFile copyFrom:  1 to: anFile  size .
	[(anFile at:1) = $\ or: [(anFile at:1) = $/]] whileTrue: [ 
		seekName := anFile copyFrom:  2 to: anFile  size ].

	offset := (seekName findString: '\' ) max: (seekName findString: '/' ).
	nextName := nil.
	offset > 0 ifTrue:[ 
		nextName := seekName  copyFrom:  offset + 1 to: seekName  size. 
		 seekName := seekName  copyFrom:  1 to:  offset -1 ].
	offset := bpb clusterAddr: anCluster.
	[true] whileTrue: [
		0 to: bpb bpbBytesPerSec * bpb bpbSecPerClust - FatDirEntry byteSize
		   by: FatDirEntry byteSize
		   do: [:i |
			entry := FatDirEntry fromBytes: (buffer copyStringFrom: offset to: offset + FatDirEntry byteSize).
			offset := offset + FatDirEntry byteSize.
			entry isEmpty ifTrue: [^ nil].
			entry isDelete ifFalse:  [ 
			entry getFullName = seekName ifTrue: [
				entry isVolume ifTrue: [ ^ nil ] .
				entry isDirectory  
					ifTrue: [nextName isNil ifTrue: [ ^ nil ] ]
					ifFalse: [ ^ self  findFile: nextName  cluster:  entry deStartCluster mode:aMode].
				 ^ entry
			] ] ].
			offset :=  (anCluster == 0)
				ifTrue: [ offset + FatDirEntry byteSize ]
				ifFalse: [ bpb clusterAddr: (bpb getFatEntry: anCluster from: buffer) ] ]!

followDir: anCluster
	| entry offset |
	offset := bpb clusterAddr: anCluster.	
	[true] whileTrue: [	
		0 to: bpb bpbBytesPerSec * bpb bpbSecPerClust - FatDirEntry byteSize
		   by: FatDirEntry byteSize
		   do: [:i |
				"self halt. Transcript show: 'offset:', offset displayString ; cr."
				entry := FatDirEntry fromBytes: (buffer copyStringFrom: offset to: offset + FatDirEntry byteSize).
				offset := offset + FatDirEntry byteSize.
				entry isEmpty ifTrue: [^-1].
				entry isDelete ifFalse: [Transcript show: entry asString; cr].
				(entry isDirectory and: [entry isDot not] ) ifTrue: [self followDir: entry deStartCluster].
				"entry := FatDirEntry fromBytes: (buffer copyStringFrom: offset to: offset + FatDirEntry byteSize)"
			  ].
			  offset :=  (anCluster == 0)
				ifTrue: [ offset + FatDirEntry byteSize ]
				ifFalse: [ bpb clusterAddr: (bpb getFatEntry: anCluster from: buffer) ] ]!

imgFile: anImgFile
	| f |
	f := File open: anImgFile.
	bpb := 
			[buffer := ByteArray new: f size.
			f read: buffer count: f size.
			btSector := BootSector33 fromBytes: buffer.
			BPB33 fromBytes: btSector bsBPB]
					ensure: [f close]! !
!Fat categoriesFor: #bpb!accessing!public! !
!Fat categoriesFor: #btSector!accessing!public! !
!Fat categoriesFor: #buffer!accessing!public! !
!Fat categoriesFor: #findFile:cluster:mode:!public! !
!Fat categoriesFor: #followDir:!public! !
!Fat categoriesFor: #imgFile:!accessing!public! !

!Fat class methodsFor!

fromImage: anImgFile
	^super new
		imgFile: anImgFile;
		yourself! !
!Fat class categoriesFor: #fromImage:!public! !

BootSector33 guid: (GUID fromString: '{8ee6851d-190e-4bcd-bfe1-35da4716c99e}')!
BootSector33 comment: ''!
!BootSector33 categoriesForClass!External-Data-Structured-Win32! !
!BootSector33 methodsFor!

bsBootCode
	"Answer the <SBYTEArray> value of the receiver's 'bsBootCode' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #bsBootCode) length: 479!

bsBootSectSig0
	"Answer the <Integer> value of the receiver's 'bsBootSectSig0' field."

	^bytes byteAtOffset: ##(self offsetOf: #bsBootSectSig0)!

bsBootSectSig1
	"Answer the <Integer> value of the receiver's 'bsBootSectSig1' field."

	^bytes byteAtOffset: ##(self offsetOf: #bsBootSectSig1)!

bsBPB
	"Answer the <SBYTEArray> value of the receiver's 'bsBPB' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #bsBPB) length: 19!

bsDriveName
	"Answer the <Integer> value of the receiver's 'bsDriveName' field."

	^bytes byteAtOffset: ##(self offsetOf: #bsDriveName)!

bsJump
	"Answer the <SBYTEArray> value of the receiver's 'bsJump' field."

	^SBYTEArray fromAddress: bytes yourAddress length: 3!

bsOemName
	"Answer the <String> value of the receiver's 'bsOemName' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #bsOemName)! !
!BootSector33 categoriesFor: #bsBootCode!**compiled accessors**!public! !
!BootSector33 categoriesFor: #bsBootSectSig0!**compiled accessors**!public! !
!BootSector33 categoriesFor: #bsBootSectSig1!**compiled accessors**!public! !
!BootSector33 categoriesFor: #bsBPB!**compiled accessors**!public! !
!BootSector33 categoriesFor: #bsDriveName!**compiled accessors**!public! !
!BootSector33 categoriesFor: #bsJump!**compiled accessors**!public! !
!BootSector33 categoriesFor: #bsOemName!**compiled accessors**!public! !

!BootSector33 class methodsFor!

defineFields
	"struct bootsector33 {
	uint8_t	bsJump[3];		/* jump inst E9xxxx or EBxx90 */
	int8_t	bsOemName[8];		/* OEM name and version */
	int8_t	bsBPB[19];		/* BIOS parameter block */
	int8_t	bsDriveNumber;		/* drive number (0x80) */
	int8_t	bsBootCode[479];	/* pad so struct is 512b */
	uint8_t	bsBootSectSig0;
	uint8_t	bsBootSectSig1;
	#define	BOOTSIG0	0x55
	#define	BOOTSIG1	0xaa }"

	self
	     defineField: #bsJump type:  (ArrayField type: SBYTEArray  length:  3) beReadOnly;
	     defineField: #bsOemName type: (StringField length:  8) beReadOnly;
	     defineField: #bsBPB type: (ArrayField type: SBYTEArray  length: 19) beReadOnly;
	     defineField: #bsDriveName type: BYTEField readOnly;
	     defineField: #bsBootCode type: (ArrayField type: SBYTEArray  length:  479) beReadOnly;
	     defineField: #bsBootSectSig0 type: BYTEField readOnly;
             defineField: #bsBootSectSig1 type: BYTEField readOnly! !
!BootSector33 class categoriesFor: #defineFields!public! !

BPB33 guid: (GUID fromString: '{53bed680-7f92-4782-8fde-e605bacf8f67}')!
BPB33 comment: ''!
!BPB33 categoriesForClass!External-Data-Structured-Win32! !
!BPB33 methodsFor!

bpbBytesPerSec
	"Answer the <Integer> value of the receiver's 'bpbBytesPerSec' field."

	^bytes wordAtOffset: 0!

bpbFATs
	"Answer the <Integer> value of the receiver's 'bpbFATs' field."

	^bytes byteAtOffset: ##(self offsetOf: #bpbFATs)!

bpbFATsecs
	"Answer the <Integer> value of the receiver's 'bpbFATsecs' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbFATsecs)!

bpbHeads
	"Answer the <Integer> value of the receiver's 'bpbHeads' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbHeads)!

bpbHiddenSecs
	"Answer the <Integer> value of the receiver's 'bpbHiddenSecs' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbHiddenSecs)!

bpbMedia
	"Answer the <Integer> value of the receiver's 'bpbMedia' field."

	^bytes byteAtOffset: ##(self offsetOf: #bpbMedia)!

bpbResSectors
	"Answer the <Integer> value of the receiver's 'bpbResSectors' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbResSectors)!

bpbRootDirEnts
	"Answer the <Integer> value of the receiver's 'bpbRootDirEnts' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbRootDirEnts)!

bpbSecPerClust
	"Answer the <Integer> value of the receiver's 'bpbSecPerClust' field."

	^bytes byteAtOffset: ##(self offsetOf: #bpbSecPerClust)!

bpbSecPerTrack
	"Answer the <Integer> value of the receiver's 'bpbSecPerTrack' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbSecPerTrack)!

bpbSectors
	"Answer the <Integer> value of the receiver's 'bpbSectors' field."

	^bytes wordAtOffset: ##(self offsetOf: #bpbSectors)!

clusterAddr: anCluster
	| offset |
	offset := self rootDirAddr.
	anCluster > 0 "MSDOSFSROOT"
		ifTrue: 
			[offset := offset + (self bpbRootDirEnts * FatDirEntry byteSize).
			offset := offset + (self bpbBytesPerSec * self bpbSecPerClust * (anCluster - 2  "CLUST_FIRST" ))].	
	^offset!

getFatEntry: anCluster from: aBuff 
	" get_fat_entry returns the value from the FAT entry for   clusternum. "

	| offset b1 b2 |
	offset := self bpbResSectors * self bpbBytesPerSec * self bpbSecPerClust + (3 * (anCluster >> 1)) .
	b1 := aBuff at: offset+1.
	b2 := aBuff at: offset + 2.
	^anCluster even
		ifTrue: [(b2 & 16r0F) << 8 | b1] 
		ifFalse: [b2 << 4 | ((b1 & 16rF0) >> 4)]!

rootDirAddr
	" offset for the start of the root direcory"

	^self bpbBytesPerSec * (self bpbResSectors + (self bpbFATs * self bpbFATsecs)) +1 ! !
!BPB33 categoriesFor: #bpbBytesPerSec!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbFATs!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbFATsecs!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbHeads!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbHiddenSecs!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbMedia!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbResSectors!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbRootDirEnts!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbSecPerClust!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbSecPerTrack!**compiled accessors**!public! !
!BPB33 categoriesFor: #bpbSectors!**compiled accessors**!public! !
!BPB33 categoriesFor: #clusterAddr:!public! !
!BPB33 categoriesFor: #getFatEntry:from:!public! !
!BPB33 categoriesFor: #rootDirAddr!public! !

!BPB33 class methodsFor!

defineFields
"struct bpb33 {
	uint16_t	bpbBytesPerSec; /* bytes per sector */
	uint8_t	bpbSecPerClust; /* sectors per cluster */
	uint16_t	bpbResSectors;	/* number of reserved sectors */
	uint8_t	bpbFATs;		/* number of FATs */
	uint16_t	bpbRootDirEnts;	/* number of root directory entries */
	uint16_t	bpbSectors;	/* total number of sectors */
	uint8_t	bpbMedia;		/* media descriptor */
	uint16_t	bpbFATsecs;	/* number of sectors per FAT */
	uint16_t	bpbSecPerTrack;	/* sectors per track */
	uint16_t	bpbHeads;	/* number of heads */
	uint16_t	bpbHiddenSecs;	/* number of hidden sectors */};"
self
	defineField: #bpbBytesPerSec type: WORDField readOnly ;
	defineField: #bpbSecPerClust type: BYTEField readOnly offset: 2;
	defineField: #bpbResSectors type: WORDField readOnly offset: 3 ;
	defineField: #bpbFATs type: BYTEField readOnly offset: 5 ;
	defineField: #bpbRootDirEnts type: WORDField readOnly offset: 6;
	defineField: #bpbSectors type: WORDField readOnly offset: 8;
	defineField: #bpbMedia type: BYTEField readOnly offset: 10 ;
	defineField: #bpbFATsecs type: WORDField readOnly offset: 11;
	defineField: #bpbSecPerTrack type: WORDField readOnly offset: 13;
	defineField: #bpbHeads type: WORDField readOnly offset: 15;
	defineField: #bpbHiddenSecs type: WORDField readOnly offset: 17

! !
!BPB33 class categoriesFor: #defineFields!public! !

FatDirEntry guid: (GUID fromString: '{6ecd0385-bc74-4d07-b542-6033621e8c8e}')!
FatDirEntry comment: ''!
!FatDirEntry categoriesForClass!External-Data-Structured! !
!FatDirEntry methodsFor!

asString
	^self deName , self deExtension
		, ' size: ' , self deFileSize displayString 
		, ' atr: ' , self deAttributes asHexString 
		, ' start cluster: ' , self deStartCluster displayString!

deADate
	"Answer the <SBYTEArray> value of the receiver's 'deADate' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #deADate) length: 2!

deAttributes
	"Answer the <Integer> value of the receiver's 'deAttributes' field."

	^bytes byteAtOffset: ##(self offsetOf: #deAttributes)!

deCDate
	"Answer the <SBYTEArray> value of the receiver's 'deCDate' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #deCDate) length: 2!

deCHundredth
	"Answer the <Integer> value of the receiver's 'deCHundredth' field."

	^bytes byteAtOffset: ##(self offsetOf: #deCHundredth)!

deCTime
	"Answer the <SBYTEArray> value of the receiver's 'deCTime' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #deCTime) length: 2!

deExtension
	"Answer the <String> value of the receiver's 'deExtension' field."

	^String fromAddress: bytes yourAddress + ##(self offsetOf: #deExtension) length: 3!

deFileSize
	"Answer the <Integer> value of the receiver's 'deFileSize' field."

	^bytes dwordAtOffset: ##(self offsetOf: #deFileSize)!

deHighClust
	"Answer the <Integer> value of the receiver's 'deHighClust' field."

	^bytes wordAtOffset: ##(self offsetOf: #deHighClust)!

deLowerCase
	"Answer the <Integer> value of the receiver's 'deLowerCase' field."

	^bytes byteAtOffset: ##(self offsetOf: #deLowerCase)!

deMDate
	"Answer the <SBYTEArray> value of the receiver's 'deMDate' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #deMDate) length: 2!

deMTime
	"Answer the <SBYTEArray> value of the receiver's 'deMTime' field."

	^SBYTEArray fromAddress: bytes yourAddress + ##(self offsetOf: #deMTime) length: 2!

deName
	"Answer the <String> value of the receiver's 'deName' field."

	^String fromAddress: bytes yourAddress length: 8!

deStartCluster
	"Answer the <Integer> value of the receiver's 'deStartCluster' field."

	^bytes wordAtOffset: ##(self offsetOf: #deStartCluster)!

getFullName
	^self deName trimBlanks , $. , self deExtension trimBlanks!

isDelete
  ^ (self deName at: 1) asciiValue == 16rE5!

isDirectory
	^(self deAttributes & 16r10) == 16r10!

isDot
	^(self deName trimBlanks = '.')   or: [self deName trimBlanks = '..']!

isEmpty
  ^ (self deName at: 1) asciiValue == 0!

isVolume
	^(self deAttributes & 16r08) == 16r08! !
!FatDirEntry categoriesFor: #asString!public! !
!FatDirEntry categoriesFor: #deADate!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deAttributes!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deCDate!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deCHundredth!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deCTime!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deExtension!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deFileSize!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deHighClust!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deLowerCase!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deMDate!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deMTime!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deName!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #deStartCluster!**compiled accessors**!public! !
!FatDirEntry categoriesFor: #getFullName!public! !
!FatDirEntry categoriesFor: #isDelete!public! !
!FatDirEntry categoriesFor: #isDirectory!public! !
!FatDirEntry categoriesFor: #isDot!public! !
!FatDirEntry categoriesFor: #isEmpty!public! !
!FatDirEntry categoriesFor: #isVolume!public! !

!FatDirEntry class methodsFor!

defineFields
	"struct direntry {
	uint8_t	deName[8];	/* filename, blank filled */
#define	SLOT_EMPTY	0x00		/* slot has never been used */
#define	SLOT_E5		0x05		/* the real value is 0xe5 */
#define	SLOT_DELETED	0xe5		/* file in this slot deleted */
	uint8_t	deExtension[3];	/* extension, blank filled */
	uint8_t	deAttributes;	/* file attributes */
#define	ATTR_NORMAL	0x00		/* normal file */
#define	ATTR_READONLY	0x01		/* file is readonly */
#define	ATTR_HIDDEN	0x02		/* file is hidden */
#define	ATTR_SYSTEM	0x04		/* file is a system file */
#define	ATTR_VOLUME	0x08		/* entry is a volume label */
#define	ATTR_DIRECTORY	0x10		/* entry is a directory name */
#define	ATTR_ARCHIVE	0x20		/* file is new or modified */
	uint8_t	deLowerCase;	/* NT VFAT lower case flags */
#define	LCASE_BASE	0x08		/* filename base in lower case */
#define	LCASE_EXT	0x10		/* filename extension in lower case */
	uint8_t	deCHundredth;	/* hundredth of seconds in CTime */
	uint8_t	deCTime[2];	/* create time */
	uint8_t	deCDate[2];	/* create date */
	uint8_t	deADate[2];	/* access date */
	uint8_t	deHighClust[2];	/* high bytes of cluster number */
	uint8_t	deMTime[2];	/* last update time */
	uint8_t	deMDate[2];	/* last update date */
	uint8_t	deStartCluster[2]; /* starting cluster of file */
	uint8_t	deFileSize[4];	/* size of file in bytes */};"

self
	defineField: #deName type: (ArrayField type: String length:  8) beReadOnly;
	defineField: #deExtension type: (ArrayField type: String length:  3) beReadOnly;
	defineField: #deAttributes type: BYTEField readOnly;
	defineField: #deLowerCase type: BYTEField readOnly;
	defineField: #deCHundredth type: BYTEField readOnly;	
	defineField: #deCTime type: (ArrayField type: SBYTEArray length:  2) beReadOnly;
	defineField: #deCDate type: (ArrayField type: SBYTEArray length:  2)beReadOnly;
	defineField: #deADate type: (ArrayField type: SBYTEArray length:  2) beReadOnly;
	defineField: #deHighClust type: WORDField readOnly;
	defineField: #deMTime type: (ArrayField type: SBYTEArray length:  2) beReadOnly;
	defineField: #deMDate type: (ArrayField type: SBYTEArray length:  2) beReadOnly;
	defineField: #deStartCluster type: WORDField readOnly;
	defineField: #deFileSize type: DWORDField readOnly! !
!FatDirEntry class categoriesFor: #defineFields!public! !

"Binary Globals"!

