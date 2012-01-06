// $Id$
// 
// Copyright (2008) Sandia Corporation. Under the terms of
// Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
// Government retains certain rights in this software.
// 
// Hermes is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// Hermes is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General
// Public License along with Hermes.  If not, see
// <http://www.gnu.org/licenses/>.
// 
// C_Groups

/*! \file PFFdataset.h
 *  \brief Defines and implements the PFF_Dataset template base class
 *         and the derived PFF_Dataset_IFL, PFF_Dataset_Nonuniform,
 *         PFF_Dataset_Uniform, and PFF_Dataset_Vertex template classes.
 */
 
#ifndef PFFdataset_h
#define PFFdataset_h 1

#ifdef CODE_MP
# define USE_MPI 1
#endif

#include "PFFfile.h"
#include "pff_ds.hh"
#include "pff_bld.hh"
#include "pff_free.hh"
#include <string>
#include <cstdlib>
#include <cstring>


//! An abstraction for a PFF dataset.
template <typename T>
class PFF_Dataset 
{
 public:

  //! Enumeration over PFF "raw" dataset types.
  typedef enum PFF_DS_Type { UF3=1, UF1, NF3, NV3, VTX, IFL, NGD, NG3, 
                               NI3 } PFF_DS_Type;

  /*! \brief Static factory method for creating dataset objects by reading
   *         them from a file.
   *
   *  \param file     PFF_File object from which to read the dataset
   *  \param dataset  Dataset to read (use 0 for the current dataset).
   *  \param keep     Indicates whether or not to keep non-zero floating
   *                  point values if underflow occurs.
   *
   *  \note There are no public constructors for creating PFF_Dataset objects
   *        (or derived clas objects) by reading them from a file. This method
   *        is the only interface available with this function.
   */  
  static PFF_Dataset<T> *Read_Dataset(PFF_File *file, int dataset=0,
                                      bool keep=false);

  //! Destructor.
  virtual ~PFF_Dataset ();

  //! Returns dataset's "raw" PFF type.
  PFF_DS_Type Raw_Type() const
  {
    return raw_type;
  }

  //! Returns the dataset's application type.
  int Application_Type() const
  {
    return ds_header->apptype;
  }

  //! Returns the dataset's title.
  std::string Title() const
  {
    return std::string(ds_header->title);
  }

  //! Returns the dataset's title.
  void Title(std::string title) const;

  //! Returns the dataset's type name.
  std::string Type_Name() const
  {
    return std::string(ds_header->type_name);
  }

  /*! \brief Returns number of spare words for the specified block.
   *
   *  \param block Block number.
   */  
  virtual int Spare_Count(int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's spare words for the
   *         specified block.
   *
   *  \param block Block number.
   */  
  virtual int *Spare_Words(int block = 0) const;

  /*! \brief Returns the size of the dataset's grid.
   *
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */  
  virtual long Grid_Length(int dir, int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's grid.
   * 
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */
  virtual T* Grid(int dir, int block = 0) const;

  //! Returns the dimensionality of the dataset's spatial grid.
  virtual int Spatial_Dimensions() const;

  //! Returns the number of data attributes for the dataset.
  virtual int Attribute_Dimensions() const;

  //! Returns the number of blocks for the dataset.
  virtual int Block_Count() const;

  /*! \brief Returns a pointer to a copy of the dataset's floating 
   *         point data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number.
   */
  virtual T* R_Data(int attribute, int block = 0) const = 0;

  /*! \brief Returns a pointer to a copy of the dataset's integer data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number.
   */
  virtual int* I_Data(int attribute, int block = 0) const = 0;

  /*! \brief Returns the length of the specified entity of data.
   * 
   *  \param entity The entity depends upon the the semantics of the 
   *                deriving class.
   */
  virtual long Data_Length(int entity = 0) const = 0;

  /*! \brief Writes the dataset to the specified file.
   * 
   *  \param file  PFF_File object associated with an open PFF file.
   *
   *  \return  PFF error status (0 for no error).
   */
  int Write(PFF_File *file) const
  {
    return file->Write_Dataset(any_dataset);
  }

  /*! \brief returns a pointer to the underlying C dataset structure.
   * 
   *  This method is provided to allow an application to access functionality
   *  of the underlying C PFF datasets that have not yet been implemented in
   *  the PFF_Dataset class or its various derived classes.
   *
   *  \note Extreme care should be exercised in using this method!
   */
  PFF::PFFds_any *Get_DSany()
  {
    return any_dataset;
  }

 protected:
  /*! \brief Constructor.
   * 
   *  \param any_ds The pointer to the "raw" PFF dataset.
   *
   *  \note "Read-from-File" contructor is not public! Static factory
   *        method PFF_Dataset::Read_Dataset should be used instead. 
   */
  PFF_Dataset(PFF::PFFds_any *any_ds);

  // Data Members for Class Attributes

  //! "Raw" PFF dataset type.
  PFF_DS_Type raw_type;

  //! pointer to header of raw PFF dataset.
  PFF::PFFhead *ds_header;

  //! The base "raw" PFF dataset pointer.
  PFF::PFFds_any *any_dataset;
};

//! An abstraction of a PFF dataset containing non-uniform grid based data.
template <typename T>
class PFF_Dataset_Nonuniform : public PFF_Dataset<T>
{
  friend PFF_Dataset<T> *PFF_Dataset<T>::Read_Dataset(PFF_File *, int, bool);

 public:
  //! Destructor.
  virtual ~PFF_Dataset_Nonuniform();

  /*! \brief Returns number of spare words for the specified block.
   *
   *  \param block Block number.
   */  
  virtual int Spare_Count(int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's spare words for the
   *         specified block.
   *
   *  \param block Block number.
   */  
  virtual int *Spare_Words(int block = 0) const;

  /*! \brief Returns the size of the dataset's grid.
   *
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */  
  virtual long Grid_Length(int dir, int block = 0) const;

  //! Returns the dimensionality of the dataset's spatial grid.      
  virtual int Spatial_Dimensions() const;

  //! Returns the number of data attributes for the dataset.
  virtual int Attribute_Dimensions() const;

  //! Returns the number of blocks for the dataset.
  virtual int Block_Count() const;

  /*! \brief Returns a pointer to a copy of the dataset's grid.
   * 
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */
  virtual T* Grid(int dir, int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's floating 
   *         point data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number.
   */
  virtual T* R_Data(int attribute, int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's integer data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number.
   */
  virtual int* I_Data(int attribute, int block = 0) const;

  /*! \brief This routine returns the length of a specified block of data.
   * 
   *  \param block Block number.
   */
  virtual long Data_Length(int block = 0) const;

 private:
  /*! \brief Constructor.
   * 
   *  \param any_ds The pointer to the "raw" PFF dataset.
   *  \param type The "raw" PFF dataset type.
   *
   *  \note "Read-from-File" contructor is not public! Static factory
   *        method PFF_Dataset::Read_Dataset should be used instead. 
   */
  PFF_Dataset_Nonuniform(PFF::PFFds_any *any_ds,
                         typename PFF_Dataset<T>::PFF_DS_Type type);

  // Data Members for Class Attributes

  //! The "raw" PFF dataset pointer.
  PFF::PFFds_nonuniform *dataset;
};

//! An abstraction of a dataset containing an integer-float list.
template <typename T>
class PFF_Dataset_IFL : public PFF_Dataset<T>
{
  friend PFF_Dataset<T> *PFF_Dataset<T>::Read_Dataset(PFF_File *, int, bool);

 public:
  //! Enumeration over the IFL attributes.
  typedef enum IFL_enum { iarray, farray, flist, IFL_enum_len } IFL_enum;

  //! Destructor.
  virtual ~PFF_Dataset_IFL();

  /*! \brief Returns the number of data attributes for the dataset.
   *
   *  Legal attribute values are: iarray, farray, or flist.
   */
  virtual int Attribute_Dimensions() const;

  /*! \brief Returns a pointer to a copy of the dataset's floating 
   *         point data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number (not used).
   */
  virtual T* R_Data(int attribute, int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's integer data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number (not used).
   */
  virtual int* I_Data(int attribute, int block = 0) const;

  /*! \brief This routine returns the length of a specified attribute
   *         of data. 
   * 
   *  \param attribute Either iarray, farray, or flist.
   */
  virtual long Data_Length(int attribute = 0) const;

 private:
  /*! \brief Constructor.
   * 
   *   \param any_ds The pointer to the "raw" PFF dataset.
   *   \param type The "raw" PFF dataset type.
   *
   *  \note "Read-from-File" contructor is not public! Static factory
   *        method PFF_Dataset::Read_Dataset should be used instead. 
   */
  PFF_Dataset_IFL (PFF::PFFds_any *any_ds,
                   typename PFF_Dataset<T>::PFF_DS_Type type);

  // Data Members for Class Attributes

  //! The "raw" PFF dataset pointer.
  PFF::PFFds_ifl *dataset;
};

//! An abstraction of a PFF dataset containing uniform grid based data.
template <typename T>
class PFF_Dataset_Uniform : public PFF_Dataset<T>
{
  friend PFF_Dataset<T> *PFF_Dataset<T>::Read_Dataset(PFF_File *, int, bool);

 public:
  //! Destructor.
  virtual ~PFF_Dataset_Uniform();

  /*! \brief Returns number of spare words for the specified block.
   *
   *  \param block Block number.
   */  
  virtual int Spare_Count(int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's spare words for the
   *         specified block.
   *
   *  \param block Block number.
   */  
  virtual int *Spare_Words(int block = 0) const;

  /*! \brief Returns the size of the dataset's grid.
   *
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */  
  virtual long Grid_Length(int dir, int block = 0) const;

  //! Returns the dimensionality of the dataset's spatial grid.      
  virtual int Spatial_Dimensions() const;

  //! Returns the number of data attributes for the dataset.
  virtual int Attribute_Dimensions() const;

  //! Returns the number of blocks for the dataset.
  virtual int Block_Count() const;

  /*! \brief Returns a pointer to a copy of the dataset's grid.
   * 
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */
  virtual T* Grid(int dir, int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's floating 
   *         point data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number.
   */
  virtual T* R_Data(int attribute, int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's integer data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number.
   */
  virtual int* I_Data(int attribute, int block = 0) const;

  /*! \brief This routine returns the length of a specified block of data.
   * 
   *  \param block Block number.
   */
  virtual long Data_Length(int block = 0) const;
 
  /*! \brief Returns the dataset's grid start value.
   * 
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */
  T X0(int dir, int block = 0) const;

  /*! \brief Returns the dataset's grid delta value.
   * 
   *  \param dir Coordinate direction.
   *  \param block Block number.
   */
  T DeltaX(int dir, int block = 0) const;

 private:
  /*! \brief Constructor.
   * 
   *  \param any_ds The pointer to the "raw" PFF dataset.
   *  \param type The "raw" PFF dataset type.
   *
   *  \note "Read-from-File" contructor is not public! Static factory
   *        method PFF_Dataset::Read_Dataset should be used instead. 
   */
  PFF_Dataset_Uniform(PFF::PFFds_any *any_ds,
                      typename PFF_Dataset<T>::PFF_DS_Type type);

  // Data Members for Class Attributes

  //! The "raw" PFF dataset pointer.
  PFF::PFFds_uniform *dataset;
};

//! An abstraction of a dataset containing a list of vertices and attributes at
//! those vertices.
template <typename T>
class PFF_Dataset_Vertex : public PFF_Dataset<T>
{
  friend PFF_Dataset<T> *PFF_Dataset<T>::Read_Dataset(PFF_File *, int, bool);

 public:
    //! Enumeration over spatial coordinate and data attribute.
  enum /* int */ { COORDINATE, ATTRIBUTE };

  /*! \brief Constructor for creating a new (empty) vertex dataset object.
   * 
   *   \param app    Application dataset type.
   *   \param title  Dataset's title.
   *   \param type   Dataset's type name.
   *   \param dims   Number of spatial dimensions for vertices.
   *   \param dimd   Number of non-spatial attributes for vertices.
   *   \param nspare Number of spare words.
   *   \param spare  Pointer to spare words.
   *   \param ierr   Returns error status of underlying PFF calls.
   */
  PFF_Dataset_Vertex (int app, std::string title, std::string type, int dims,
                      int dimd, int nspare, int *spare, int &ierr);

  //! Destructor.
  virtual ~PFF_Dataset_Vertex();

  /*! \brief Returns number of spare words for the specified block.
   *
   *  \param block Block number.
   */  
  virtual int Spare_Count(int block = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's spare words for the
   *         specified block.
   *
   *  \param block Block number.
   */  
  virtual int *Spare_Words(int block = 0) const;

  //! Returns the dimensionality of the dataset's vertices.      
  virtual int Spatial_Dimensions() const;

  //! Returns the number of attributes for the dataset's vertices.
  virtual int Attribute_Dimensions() const;

  /*! \brief Returns a pointer to a copy of the dataset's floating 
   *         point data.
   *
   *  \param index  Coordinate or attribute index. If type is zero,
   *                spatial locations of the vertices in the specified
   *                coordinate are returned. If type is nonzero, values
   *                for the corresponding attribute of the vertices for
   *                are returned.
   *  \param type   Spatial coordinates (0) or data attributes (!0).
   */
  virtual T* R_Data(int index, int type = 0) const;

  /*! \brief Returns a pointer to a copy of the dataset's integer data.
   *
   *  \param attribute Attribute index.
   *  \param block Block number (not used).
   *
   *  \note Since their is no integer data associated with Vertex objects,
   *        this will always return 0;
   */
  virtual int* I_Data(int attribute, int block = 0) const
  {
    return 0;
  }

  /*! \brief This routine returns the number of vertices.
   * 
   *  \param attribute Attribute index (not used).
   */
  virtual long Data_Length(int attribute = 0) const;

  /*! \brief This routine sets the number of vertices.
   * 
   *  \param len  Number of vertices.
   */
  void Set_Data_Length(long len);

  /*! \brief Sets the label for the specified coordinate or attribute.
   *
   *  \param index  Coordinate or attribute index. If type is zero,
   *                spatial locations of the vertices in the specified
   *                coordinate are returned. If type is nonzero, values
   *                for the corresponding attribute of the vertices for
   *                are returned.
   *  \param type   Spatial coordinates (0) or data attributes (!0).
   */
  void Set_Label(std::string label, int index, int type = 0);

  /*! \brief Sets the dataset data for the specified coordinate or attribute.
   *
   *  \param index  Coordinate or attribute index. If type is zero,
   *                spatial locations of the vertices in the specified
   *                coordinate are returned. If type is nonzero, values
   *                for the corresponding attribute of the vertices for
   *                are returned.
   *  \param type   Spatial coordinates (0) or data attributes (!0).
   */
  void Fill_Data(const T *array, int index, int type = 0);

  //! Clears the data for all spatial coordinates and vertex attributes.
  void Clear_Data();

  /*! \brief  Returns pointers to the vertex coordinate and attribute
   *          arrays that are consistent with current dimensionality and 
   *          vertex count.
   *
   *  \param vert   Reference to pointer for the vertex coordinate data
   *  \param data   Reference to pointer for the vertex attribute data
   *
   *  \note  Vertex coordinate data is stored in a linear one-dimesional
   *         array. The coodinate value for the ith coordinate direction of
   *         the jth vertex is stored in vert[i+ndims*j], where ndims is
   *         the number of spatial dimensions. Vertex attribute data is
   *         stored in a two-dimesional format -- data[k][j] contains the
   *         value for the kth attribute of the jth vertex.
   */
  void Get_Data_Ptrs(float* &vert, float** &data);

 private:
  /*! \brief Constructor.
   * 
   *  \param any_ds The pointer to the "raw" PFF dataset.
   *  \param type The "raw" PFF dataset type.
   *
   *  \note "Read-from-File" contructor is not public! Static factory
   *        method PFF_Dataset::Read_Dataset should be used instead. 
   */
  PFF_Dataset_Vertex (PFF::PFFds_any *any_ds,
                      typename PFF_Dataset<T>::PFF_DS_Type type);

  // Data Members for Class Attributes

  //! The "raw" PFF dataset pointer.
  PFF::PFFds_vertex *dataset;
};

/****************************************************************************/
template <typename T>
PFF_Dataset<T> *PFF_Dataset<T>::Read_Dataset(PFF_File *file, int dataset,
                                             bool keep)
/****************************************************************************/
{
  int ptype;
  PFF::PFFds_any *pff_ds = file->Read_Dataset( ptype, dataset, keep );
  if ( !pff_ds ) return 0;

  PFF_DS_Type type;

  switch (ptype)  {
  case PFF::PFTUF3:   type = PFF_Dataset::UF3; break;
  case PFF::PFTUF1:   type = PFF_Dataset::UF1; break;
  case PFF::PFTNF3:   type = PFF_Dataset::NF3; break;
  case PFF::PFTNV3:   type = PFF_Dataset::NV3; break;
  case PFF::PFTNG3:   type = PFF_Dataset::NG3; break;
  case PFF::PFTNI3:   type = PFF_Dataset::NI3; break;
  case PFF::PFTNGD:   type = PFF_Dataset::NGD; break;
  case PFF::PFTVTX:   type = PFF_Dataset::VTX; break;
  case PFF::PFTIFL:   type = PFF_Dataset::IFL; break;
  default: return 0;
  }

  switch (ptype)  {
  case PFF::PFTUF3:
  case PFF::PFTUF1:
    return new PFF_Dataset_Uniform<T>(pff_ds, type);
  case PFF::PFTNF3:
  case PFF::PFTNV3:
  case PFF::PFTNG3:
  case PFF::PFTNI3:
  case PFF::PFTNGD:
    return new PFF_Dataset_Nonuniform<T>(pff_ds, type);
  case PFF::PFTVTX:
    return new PFF_Dataset_Vertex<T>(pff_ds, type);
  case PFF::PFTIFL:
    return new PFF_Dataset_IFL<T>(pff_ds, type);
  }
  return 0;
}

/****************************************************************************/
template <typename T>
PFF_Dataset<T>::PFF_Dataset(PFF::PFFds_any *any_ds)
  : any_dataset(any_ds)
/****************************************************************************/
{
}

/****************************************************************************/
template <typename T>
PFF_Dataset<T>::~PFF_Dataset()
/****************************************************************************/
{
}

/****************************************************************************/
template <typename T>
int PFF_Dataset<T>::Spare_Count(int block) const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
int *PFF_Dataset<T>::Spare_Words(int block) const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset<T>::Grid_Length(int dir, int block) const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset<T>::Grid(int dir, int block) const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset<T>::Spatial_Dimensions() const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset<T>::Attribute_Dimensions() const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
void PFF_Dataset<T>::Title(std::string title) const
/****************************************************************************/
{
  if ( ds_header->title != 0 ) std::free(ds_header->title);
  char *p = (char *) std::malloc(title.size()+1);
  std::strcpy(p,title.c_str());
  ds_header->title = p;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset<T>::Block_Count() const
/****************************************************************************/
{
  return 1;
}


// Class PFF_Dataset_Nonuniform 

/****************************************************************************/
template <typename T>
PFF_Dataset_Nonuniform<T>::PFF_Dataset_Nonuniform
(PFF::PFFds_any *any_ds, typename PFF_Dataset<T>::PFF_DS_Type type)
  : PFF_Dataset<T>(any_ds), dataset(0)
/****************************************************************************/
{
  dataset = (PFF::PFFds_nonuniform *) any_ds;
  PFF_Dataset<T>::raw_type = type;
  PFF_Dataset<T>::ds_header = dataset->head;
}

/****************************************************************************/
template <typename T>
PFF_Dataset_Nonuniform<T>::~PFF_Dataset_Nonuniform()
/****************************************************************************/
{
  int ierr = 0;
  PFF::pf_free_nonuniform( PFF_Dataset<T>::any_dataset, &ierr );
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Nonuniform<T>::Spare_Count(int block) const
/****************************************************************************/
{
  return (dataset->block[block])->nspare;
}

/****************************************************************************/
template <typename T>
int *PFF_Dataset_Nonuniform<T>::Spare_Words(int block) const
/****************************************************************************/
{
  int len = (dataset->block[block])->nspare;
  int *x = dataset->block[block]->spare;
  int *spare = new int[len];
  for(int i=0;i<len;++i) spare[i] = x[i];

  return spare;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset_Nonuniform<T>::Grid_Length(int dir, int block) const
/****************************************************************************/
{
  return (dataset->block[block])->nx[dir];
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Nonuniform<T>::Spatial_Dimensions() const
/****************************************************************************/
{
  return dataset->dims;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Nonuniform<T>::Attribute_Dimensions() const
/****************************************************************************/
{
  return dataset->dimd;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Nonuniform<T>::Block_Count() const
/****************************************************************************/
{
  return dataset->nblk;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset_Nonuniform<T>::Grid(int dir, int block) const
/****************************************************************************/
{
  int i = 0;
  long len = Grid_Length(dir,block);
  float *x = dataset->block[block]->x[dir];
  T *grid = new T[len];

  for(i=0; i<len; i++)  {
    grid[i] = x[i];
  }
  return grid;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset_Nonuniform<T>::R_Data(int attribute, int block) const
/****************************************************************************/
{
  int i;
  
  if ( dataset->block[block]->data == 0 ) return 0;
  
  long len = Data_Length(block);
  
  float *d = dataset->block[block]->data[attribute];
  T *data = new T[len];
  
  for(i=0; i<len; i++)  {
    data[i] = d[i];
  }
  return data;
}

/****************************************************************************/
template <typename T>
int* PFF_Dataset_Nonuniform<T>::I_Data(int attribute, int block) const
/****************************************************************************/
{
  int i;
  
  if ( dataset->block[block]->idata == 0 ) return 0;
  
  long len = Data_Length(block);
  
  int *id = dataset->block[block]->idata[attribute];
  int *data = new int[len];
  
  for(i=0; i<len; i++)  {
    data[i] = id[i];
  }
  return data;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset_Nonuniform<T>::Data_Length(int block) const
/****************************************************************************/
{
  int  dir;
  long len = 1;

  for (dir=0; dir<Spatial_Dimensions(); dir++ )  len *= Grid_Length(dir,block);
  return len;
}


// Class PFF_Dataset_IFL 

/****************************************************************************/
template <typename T>
PFF_Dataset_IFL<T>::PFF_Dataset_IFL
(PFF::PFFds_any *any_ds, typename PFF_Dataset<T>::PFF_DS_Type type)
  : PFF_Dataset<T>(any_ds), dataset(0)
/****************************************************************************/
{
  dataset = (PFF::PFFds_ifl *) any_ds;
  PFF_Dataset<T>::raw_type = type;
  PFF_Dataset<T>::ds_header = dataset->head;
}

/****************************************************************************/
template <typename T>
PFF_Dataset_IFL<T>::~PFF_Dataset_IFL()
/****************************************************************************/
{
  int ierr = 0;
  PFF::pf_free_ifl( PFF_Dataset<T>::any_dataset, &ierr );
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_IFL<T>::Attribute_Dimensions() const
/****************************************************************************/
{
  return IFL_enum_len;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset_IFL<T>::R_Data(int attribute, int block) const
/****************************************************************************/
{
  int i;
  float *d = 0;

  switch (attribute)  {
  case PFF_Dataset_IFL<T>::farray:
    d = dataset->farr;
    break;
  case PFF_Dataset_IFL<T>::flist:
    d = dataset->flist;
    break;
  default:
    return 0;
  }
  
  long len = Data_Length(attribute);
  if ( len == 0 ) return 0;

  T *data = new T[len];
  
  for(i=0; i<len; i++)  {
    data[i] = d[i];
  }
  return data;
}

/****************************************************************************/
template <typename T>
int* PFF_Dataset_IFL<T>::I_Data(int attribute, int block) const
/****************************************************************************/
{
  int i;
  int *id = 0;

  switch (attribute)  {
  case PFF_Dataset_IFL<T>::iarray:
    id = dataset->iarr;
    break;
  default:
    return 0;
  }
  
  long len = Data_Length(attribute);
  if ( len == 0 ) return 0;

  int *data = new int[len];
  
  for(i=0; i<len; i++)  {
    data[i] = id[i];
  }
  return data;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset_IFL<T>::Data_Length(int attribute) const
/****************************************************************************/
{
  switch (attribute)  {
  case PFF_Dataset_IFL<T>::iarray:
    return dataset->ni;
  case PFF_Dataset_IFL<T>::farray:
    return dataset->nf;
  case PFF_Dataset_IFL<T>::flist:
    return dataset->nflt;
  default:
    return -1;
  }
}


// Class PFF_Dataset_Vertex 

/****************************************************************************/
template <typename T>
PFF_Dataset_Vertex<T>::PFF_Dataset_Vertex
(PFF::PFFds_any *any_ds, typename PFF_Dataset<T>::PFF_DS_Type type)
  : PFF_Dataset<T>(any_ds), dataset(0)
/****************************************************************************/
{
  dataset = (PFF::PFFds_vertex *) any_ds;
  PFF_Dataset<T>::raw_type = type;
  PFF_Dataset<T>::ds_header = dataset->head;
}

/****************************************************************************/
template <typename T>
PFF_Dataset_Vertex<T>::PFF_Dataset_Vertex(int app, std::string title,
                                          std::string type, int dims, int dimd,
                                          int nspare, int *spare, int &ierr)
  : PFF_Dataset<T>(0), dataset(0)
/****************************************************************************/
{
  char *tit = const_cast<char *>(title.c_str());
  char *typ = const_cast<char *>(type.c_str());
  dataset = PFF::pf_bld_vertex(app, tit, typ, dims, dimd, 0, nspare, 0,
                                     spare, 0, 0, 0, 0, 0, 0, 1, 1, 1, &ierr);
  dataset->alloc_method = 0;

  PFF_Dataset<T>::any_dataset = (PFF::PFFds_any *) dataset;
  PFF_Dataset<T>::raw_type = PFF_Dataset<T>::VTX;
  PFF_Dataset<T>::ds_header = dataset->head;
}

/****************************************************************************/
template <typename T>
PFF_Dataset_Vertex<T>::~PFF_Dataset_Vertex()
/****************************************************************************/
{
  int ierr = 0;
  PFF::pf_free_vertex( PFF_Dataset<T>::any_dataset, &ierr );
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Vertex<T>::Spare_Count(int block) const
/****************************************************************************/
{
  return dataset->nspare;
}

/****************************************************************************/
template <typename T>
int *PFF_Dataset_Vertex<T>::Spare_Words(int block) const
/****************************************************************************/
{
  int len = dataset->nspare;
  int *x = dataset->spare;
  int *spare = new int[len];
  for(int i=0;i<len;++i) spare[i] = x[i];

  return spare;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Vertex<T>::Spatial_Dimensions() const
/****************************************************************************/
{
  return dataset->dims;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Vertex<T>::Attribute_Dimensions() const
/****************************************************************************/
{
  return dataset->dimd;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset_Vertex<T>::R_Data(int index, int type) const
/****************************************************************************/
{
  int i, j;
  float *d = 0;

  long len = dataset->nv;
  if ( len == 0 ) return 0;

  T *data = new T[len];
  
  switch (type)  {
  case PFF_Dataset_Vertex<T>::COORDINATE:
    d = dataset->vert;
    j = index;
    for(i=0; i<len; i++)  {
      data[i] = d[j];
      j += dataset->dims;
    }
    break;
  case PFF_Dataset_Vertex<T>::ATTRIBUTE:
    d = dataset->data[index];
    for(i=0; i<len; i++) data[i] = d[i];
    break;
  default:
    return 0;
  }
  
  return data;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset_Vertex<T>::Data_Length(int attribute) const
/****************************************************************************/
{
  return dataset->nv;
}

/****************************************************************************/
template <typename T>
void PFF_Dataset_Vertex<T>::Set_Data_Length(long len)
/****************************************************************************/
{
  dataset->nv = len;
}

/****************************************************************************/
template <typename T>
void PFF_Dataset_Vertex<T>::Set_Label(std::string label, int index, int type)
/****************************************************************************/
{
  char ***lab_ptr;
  int dim;

  switch (type)  {
  case PFF_Dataset_Vertex<T>::COORDINATE:
    lab_ptr = &(dataset->vlabel);
    dim = dataset->dims;
    break;
  case PFF_Dataset_Vertex<T>::ATTRIBUTE:
    lab_ptr = &(dataset->dlabel);
    dim = dataset->dimd;
    break;
  default:
    return;
  }
  if ( index < 0 || index >= dim ) return;

  char *p = (char *) std::malloc(label.size()+1);
  std::strcpy(p,label.c_str());

  if ( *lab_ptr == 0 ) *lab_ptr = (char **) std::malloc(dim*sizeof(char *));
  (*lab_ptr)[index] = p;
}

/****************************************************************************/
template <typename T>
void PFF_Dataset_Vertex<T>::Clear_Data()
/****************************************************************************/
{
  int lmax;

  if ( dataset->vert ) std::free(dataset->vert);
  dataset->vert = 0;
  if ( dataset->data ) {
    switch ( dataset->alloc_method )  {
      case  1:  lmax = 1; break;
      case  2:  lmax = 0; break;
      default:  lmax = dataset->dimd; break;
    }
    for(int i=0; i<lmax; ++i)
      if ( dataset->data[i] ) std::free(dataset->data[i]);
    std::free(dataset->data);
    dataset->data = 0;
  }
  return;
}

/****************************************************************************/
template <typename T>
void PFF_Dataset_Vertex<T>::Fill_Data(const T *array, int index, int type)
/****************************************************************************/
{
  float *d;
  int i,j;
  int len = dataset->nv;
  int dim = dataset->dims;
  if (type ==  PFF_Dataset_Vertex<T>::ATTRIBUTE ) dim = dataset->dimd;
  if (len <= 0 || index < 0 || index >= dim ) return; 

  switch (type)  {
  case PFF_Dataset_Vertex<T>::COORDINATE:
    if ( dataset->vert == 0 )
      dataset->vert = (float *) std::malloc(dim*len*sizeof(float));
    d = dataset->vert;
    j = index;
    for(i=0; i<len; i++)  {
      d[j] = (float) array[i];
      j += dataset->dims;
    }
    break;
  case PFF_Dataset_Vertex<T>::ATTRIBUTE:
    if ( dataset->data == 0 ) {
      dataset->data = (float **) std::malloc(dim*sizeof(float *));
      for(i=0;i<dim;++i) dataset->data[i] = 0;
    }
    if ( dataset->data[index] != 0 ) std::free(dataset->data[index]);
    d = (float *) std::malloc(len*sizeof(float));
    dataset->data[index] = d;
    for(i=0; i<len; i++) d[i] = (float) array[i];
    break;
  default:
    return;
  }
}

/****************************************************************************/
template <typename T>
void PFF_Dataset_Vertex<T>::Get_Data_Ptrs(float* &vert, float** &data)
/****************************************************************************/
{
  Clear_Data();
  vert = 0;
  data = 0;

  long nv = dataset->nv;
  if ( nv < 1 ) return;

  int dims = dataset->dims;
  int dimd = dataset->dimd;

  dataset->alloc_method = 0;
  if ( dims > 0 ) vert = (float *) std::malloc(dims*nv*sizeof(float));

  if ( dimd > 0 ) {
    data = (float **) std::malloc(dimd*sizeof(float *));
    for(int i=0; i<dimd; ++i) data[i] = (float *)std::malloc(nv*sizeof(float));
  }
  dataset->vert = vert;
  dataset->data = data;

  return;
}


// Class PFF_Dataset_Uniform 

/****************************************************************************/
template <typename T>
PFF_Dataset_Uniform<T>::PFF_Dataset_Uniform
(PFF::PFFds_any *any_ds, typename PFF_Dataset<T>::PFF_DS_Type type)
  : PFF_Dataset<T>(any_ds), dataset(0)
/****************************************************************************/
{
  dataset = (PFF::PFFds_uniform *) any_ds;
  PFF_Dataset<T>::raw_type = type;
  PFF_Dataset<T>::ds_header = dataset->head;
}

/****************************************************************************/
template <typename T>
PFF_Dataset_Uniform<T>::~PFF_Dataset_Uniform()
/****************************************************************************/
{
  int ierr = 0;
  PFF::pf_free_uniform( PFF_Dataset<T>::any_dataset, &ierr );
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Uniform<T>::Spare_Count(int block) const
/****************************************************************************/
{
  return (dataset->block[block])->nspare;
}

/****************************************************************************/
template <typename T>
int *PFF_Dataset_Uniform<T>::Spare_Words(int block) const
/****************************************************************************/
{
  int len = (dataset->block[block])->nspare;
  int *x = dataset->block[block]->spare;
  int *spare = new int[len];
  for(int i=0;i<len;++i) spare[i] = x[i];

  return spare;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset_Uniform<T>::Grid_Length(int dir, int block) const
/****************************************************************************/
{
  return (dataset->block[block])->nx[dir];
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Uniform<T>::Spatial_Dimensions() const
/****************************************************************************/
{
  return dataset->dims;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Uniform<T>::Attribute_Dimensions() const
/****************************************************************************/
{
  return dataset->dimd;
}

/****************************************************************************/
template <typename T>
int PFF_Dataset_Uniform<T>::Block_Count() const
/****************************************************************************/
{
  return dataset->nblk;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset_Uniform<T>::Grid(int dir, int block) const
/****************************************************************************/
{
  int i = 0;
  long len = Grid_Length(dir,block);
  if ( len < 1 )  return 0;

  float dx = dataset->block[block]->dx[dir];
  T *grid = new T[len];

  grid[0] = dataset->block[block]->x0[dir];
  for(i=1; i<len; i++)  {
    grid[i] = grid[0] + i*dx;
  }
  return grid;
}

/****************************************************************************/
template <typename T>
T* PFF_Dataset_Uniform<T>::R_Data(int attribute, int block) const
/****************************************************************************/
{
  int i;
  
  if ( dataset->block[block]->data == 0 ) return 0;
  
  long len = Data_Length(block);
  
  float *d = dataset->block[block]->data[attribute];
  T *data = new T[len];
  
  for(i=0; i<len; i++)  {
    data[i] = d[i];
  }
  return data;
}

/****************************************************************************/
template <typename T>
int* PFF_Dataset_Uniform<T>::I_Data(int attribute, int block) const
/****************************************************************************/
{
  return 0;
}

/****************************************************************************/
template <typename T>
long PFF_Dataset_Uniform<T>::Data_Length(int block) const
/****************************************************************************/
{
  int  dir;
  long len = 1;

  for (dir=0; dir<Spatial_Dimensions(); dir++)  len *= Grid_Length(dir,block);
  return len;
}

/****************************************************************************/
template <typename T>
T PFF_Dataset_Uniform<T>::X0 (int dir, int block) const
/****************************************************************************/
{
  return (dataset->block[block])->x0[dir];
}

/****************************************************************************/
template <typename T>
T PFF_Dataset_Uniform<T>::DeltaX(int dir, int block) const
/****************************************************************************/
{
  return (dataset->block[block])->dx[dir];
}
#endif
