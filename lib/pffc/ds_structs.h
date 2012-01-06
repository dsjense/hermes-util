/*
-------------------------------------------------------------------------------
    PFF I/O utility:   ds_structs.h
-------------------------------------------------------------------------------
    $Id$
    
    Copyright (2008) Sandia Corporation. Under the terms of
    Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
    Government retains certain rights in this software.
    
    Hermes is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.
    
    Hermes is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General
    Public License along with Hermes.  If not, see
    <http://www.gnu.org/licenses/>.
    
-------------------------------------------------------------------------------
*/

#ifndef DS_STRUCTS_H
#define DS_STRUCTS_H

#include "pff.h"

#define     VTX_MAXSPARE    (5)
#define     VTX_MAXSPARE    (5)
#define    UNIF_MAXSPARE    (5)
#define NONUNIF_MAXSPARE    (5)

extern int      PFF_legal_uniform[];
extern int      PFF_legal_nonuniform[];
extern int      PFF_fixed_spare_length[];
extern int      PFF_ds_dims[];
extern int      PFF_ds_dimd[];

typedef struct s_PFFds_any             PFFds_any;
typedef struct s_PFFds_dir             PFFds_dir;
typedef struct s_PFFds_ifl             PFFds_ifl;
typedef struct s_PFFds_uniform         PFFds_uniform;
typedef struct s_PFFblock_uniform      PFFblock_uniform;
typedef struct s_PFFds_nonuniform      PFFds_nonuniform;
typedef struct s_PFFblock_nonuniform   PFFblock_nonuniform;
typedef struct s_PFFds_vertex          PFFds_vertex;

struct s_PFFds_any    {       /* minimal structure for any dataset */
 
  int         type;           /* Raw dataset type */
  PFFhead    *head;           /* Pointer to dataset header structure */
};

struct s_PFFds_dir    {       /* Directory dataset structure */
 
  int         type;           /* Raw dataset type */
  PFFhead    *head;           /* Pointer to dataset header structure */
  int         ref_type;       /* Raw type of referenced dataset */
  long        length;         /* Length of referenced dataset */
  long        offset;         /* File offset of referenced dataset */
};

struct s_PFFds_ifl    {       /* Integer/Float/List dataset structure */
 
  int         type;           /* Raw dataset type */
  PFFhead    *head;           /* Pointer to dataset header structure */
  long        ni;             /* Length of integer array */
  long        nf;             /* Length of float array (limited precision) */
  long        nflt;           /* Length of float list (full precision) */
  int        *iarr;           /* Pointer to integer array */
  float      *farr;           /* Pointer to float array */
  int         faoff10;        /* power-of-ten offset for float array */
  float      *flist;          /* Pointer to float list */
  int        *floff10;        /* Pointer to array of power-of-ten offsets for 
                                 values in float list */
};

struct s_PFFds_uniform    {   /* "Data on uniform grid" dataset structure */
 
  int         type;           /* Raw dataset type */
  PFFhead    *head;           /* Pointer to dataset header structure */
  int         nblk;           /* # of blocks of uniform grid data */
  int         dims;           /* dimensionality of grid */
  int         dimd;           /* dimensionality of data
                                 1D scalar (uf1)   --   dims=1 , dimd=1
                                 3D scalar (uf3)   --   dims=3 , dimd=1
                                 3D vector (uv3)   --   dims=3 , dimd=3
                                 etc.    */
  PFFblock_uniform
            **block;          /* Pointer to array of block pointers */
};

struct s_PFFblock_uniform {  /* "Data on uniform grid" single-block structure */
 
  int         nspare;   /* Number of non-default spare integers */
  int        *spare;    /* Pointer to non-default spare integers */
  long       *nx;       /* Number of grid points in each grid dimension */
  char      **glabel;   /* Pointer to array of labels in each grid dimension */
  char      **dlabel;   /* Pointer to array of labels in each data dimension */
  char       *blabel;   /* Pointer to label for this block */
  float      *x0;       /* Pointer to initial grid value in each grid 
                           dimension */
  float      *dx;       /* Pointer to grid spacing in each grid dimension */
  int         goff10;   /* power-of-ten offset for grid values */
  float     **data;     /* Pointer to array of data array pointers for each 
                           data dimension */
  int         foff10;   /* power-of-ten offset for data values */
  int         alloc_method;   /* method of allocation for data arrays:
                                 0 - data & data[0..nblk] allocated separately
                                 1 - data & data[0] allocated separately
                                 2 - data only allocated
                                 DFAULT - Don't free data at all  */       
};

struct s_PFFds_nonuniform  {  /* "Data on nonuniform grid" dataset structure */
 
  int         type;           /* Raw dataset type */
  PFFhead    *head;           /* Pointer to dataset header structure */
  int         nblk;           /* # of blocks of uniform grid data */
  int         dims;           /* dimensionality of grid */
  int         dimd;           /* dimensionality of data
                                 1D scalar (nf1)   --   dims=1 , dimd=1
                                 3D grid   (ng3)   --   dims=3 , dimd=0
                                 3D scalar (nf3)   --   dims=3 , dimd=1
                                 3D vector (nv3)   --   dims=3 , dimd=3
                                 etc.    */
  PFFblock_nonuniform
            **block;          /* Pointer to array of block pointers */
};

struct s_PFFblock_nonuniform {  
                        /* "Data on nonuniform grid" single-block structure */
 
  int         nspare;   /* Number of non-default spare integers */
  int        *spare;    /* Pointer to non-default spare integers */
  long       *nx;       /* Number of grid points in each grid dimension */
  char      **glabel;   /* Pointer to array of labels in each grid dimension */
  char      **dlabel;   /* Pointer to array of labels in each data dimension */
  char       *blabel;   /* Pointer to label for this block */
  float     **x;        /* Pointer to array of grid value arrays for each grid 
                           dimension */
  int         goff10;   /* power-of-ten offset for grid values */
  float     **data;     /* Pointer to array of data array pointers for each 
                           data dimension for float data */
  int       **idata;    /* Pointer to array of data array pointers for each 
                           data dimension for integer data */
  int         foff10;   /* power-of-ten offset for data values */
  int         alloc_method;   /* method of allocation for grid and data arrays:
                                 0 - data & data[0..nblk] allocated separately
                                 1 - data & data[0] allocated separately
                                 2 - data only allocated
                                 DFAULT - Don't free grid or data at all  */
};

struct s_PFFds_vertex  {      /* "Vertex & data" dataset structure */
 
  int         type;           /* Raw dataset type */
  PFFhead    *head;           /* Pointer to dataset header structure */
  int         dims;           /* dimensionality of grid */
  int         dimd;           /* dimensionality of data */
  int         alloc_method;   /* method of allocation for data array:
                                 0 - data & data[0..dimd] allocated separately
                                 1 - data & data[0] allocated separately
                                 2 - data only allocated  */       
  int         nspare;   /* Number of non-default spare integers */
  int        *spare;    /* Pointer to non-default spare integers */
  long        nv;             /* # of verticies */
  char      **vlabel;   /* Pointer to array of labels in each grid dimension */
  char      **dlabel;   /* Pointer to array of labels in each data dimension */
  float      *vert;     /* Pointer to array of verticies */
  int         voff10;   /* power-of-ten offset for data values */
  float     **data;     /* Pointer to array of data array pointers for each 
                           data dimension */
  int        *aoff10;   /* Pointer to array of power-of-ten offsets for data 
                                 in each data dimension */
};

typedef union  s_PFFds                 PFFds;

union s_PFFds      {         /* union of all known general dataset structures */
 
  int                type;            /* Raw dataset type */
  PFFds_any          any;             /* type and header */
  PFFds_dir          dir;             /* directory dataset type structure */
  PFFds_ifl          ifl;             /* ifl dataset type structure */
  PFFds_uniform      uniform;         /* uniform dataset type structure */
  PFFds_nonuniform   nonuniform;      /* nonuniform dataset type structure */
  PFFds_vertex       vertex;          /* vertex dataset type structure */
};

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  typedef void        (*PFFfree)  ( PFFds_any*, int* );
  typedef PFFds_any  *(*PFFread)  ( PFFfid*, int, int* );
  typedef void        (*PFFwrite) ( PFFfid*, PFFds_any*, int* );

#else

  typedef void        (*PFFfree)  ( );
  typedef PFFds_any  *(*PFFread)  ( );
  typedef void        (*PFFwrite) ( );

#endif

#define PFFMAXDS        (9)

EXTERN PFFfree   free_functs[];

EXTERN PFFread   read_functs[];

EXTERN PFFwrite  write_functs[];

#endif
