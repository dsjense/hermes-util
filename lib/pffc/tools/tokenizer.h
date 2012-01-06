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
// ------------------------------------------------------------------
// 
// The Hermes Tokenizer and Token_Iterator classes are based upon the
// Boost tokenizer and token_iterator classes, respectively.
//
// (c) Copyright Jeremy Siek and John R. Bandela 2001. 
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

/*! \file tokenizer.h
 *  \brief Interface definitions for the Tokenizer<TokenizerFunc> class, and
 *         its iterator class, Token_Iterator<TokenizerFunc>.
 */
 
#ifndef tokenizerH
#define tokenizerH
 
#include <cassert>
#include <string>
#include <stdexcept>

typedef std::string::const_iterator Iterator;
typedef std::string Type;

/*! \brief Iterator class for Tokenizer class.
 *
 *  Modelled after the token_iterator class in boost -- \n
 *  copyright John R. Bandela 2001 \n
 *  See http://www.boost.org/libs/tokenizer for documentation.
 */
template <class TokenizerFunc>
class Token_Iterator
{
 private:
  typedef Token_Iterator<TokenizerFunc> my_type;

 public:
  //! \name Constructors
  //@{
  /*! \brief Default Constructor
   *  
   *  Constructs an empty iterator object
   */
  Token_Iterator() : begin_(), end_(), valid_(false), tok_() { }

  /*! \brief Constructs a full iterator object
   *
   *  \param f     TokenizerFunc object for syntax control of token parsing.
   *  \param begin A string iterator indicating the start of stream to be
   *               parsed.
   *  \param e     A string iterator indicating the end of stream to be parsed.
   */
  Token_Iterator(TokenizerFunc f, Iterator begin, Iterator e = Iterator())
    : f_(f), begin_(begin), end_(e), valid_(false), tok_()
  {
    initialize();
  }

  /*! \brief Constructs iterator without a TokenizerFunc object
   *
   *  \param begin String iterator indicating the start of stream to be parsed.
   *  \param e     String iterator indicating the end of stream to be parsed.
   */
  Token_Iterator(Iterator begin, Iterator e = Iterator())
    : f_(), begin_(begin), end_(e), valid_(false), tok_()
  {
    initialize();
  }
  //@}

  //! prefix ++ operator
  my_type& operator++()
  {
    increment();
    return *this;
  }
  //! postfix ++ operator
  my_type operator++(int)
  {
    my_type tmp = *this;
    increment();
    return tmp;
  }

  //! Dereference operator
  Type& operator*()
  {
    return dereference();
  }

  //! Equality operator
  bool operator==(const my_type& other) const
  {
    return valid_ == other.valid_ && begin_ == other.begin_;
  }

 private:
  /*! \brief Operator to increment the iterator
   *
   *  It operates by calling the TokenizerFunc::operator() method, which
   *  parses the next token using the rules in the TokenizerFunc f object.
   *  It is used by the prefix++ and postfix++ operators.
   */
  void increment()
  {
    assert(valid_);
    valid_ = f_(begin_, end_, tok_);
  }

  //! Work method to dereference a Token_Iterator object.
  Type& dereference()
  {
    assert(valid_);
    return tok_;
  }

  //! Work method to initialize a Token_Iterator object. Used by constructors.
  void initialize()
  {
    if(valid_) return;
    f_.reset();
    valid_ = (begin_ != end_) ? f_(begin_,end_,tok_) : false;
  }

  //! Object that knows how to perform parsing for a Tokenizer object
  TokenizerFunc f_;

  //! Object that knows how to perform parsing for a Tokenizer object
  Iterator begin_;

  //! \brief std::string::iterator that points to the current position in the 
  //!        token input stream
  Iterator end_;

  //! \brief If true, it indicates that the iterator has been initialized and
  //!        is /b not at at the end of the input stream (begin_ != end_)
  bool valid_;

  //! The token that is returned when this object is dereferenced.
  Type tok_;
};


/*! \brief Tokenizer class.
 *
 *  Modelled after the tokenizer class in boost -- \n
 *  copyright John R. Bandela 2001 \n
 *  See http://www.boost.org/libs/tokenizer for documentation.
 */
template < typename TokenizerFunc > 
class Tokenizer {
 private:
  //! Alias for string iterator
  typedef std::string::const_iterator Iterator;
  //! Alias for string
  typedef std::string Container;
        
 public:
  //! typedef so Tokenizer<xxx>::iterator returns the correct iterator for
  //! this class
  typedef Token_Iterator<TokenizerFunc> iterator;

  //! \name Constructors
  //@{
  /*! \brief Constructs a tokenizer object using:
   *
   *  \param first String iterator indicating the start of stream to be parsed.
   *  \param last  String iterator indicating the end of stream to be parsed.
   *  \param f     TokenizerFunc object for syntax control of token parsing.
   */
  Tokenizer(Iterator first, Iterator last,
            const TokenizerFunc& f = TokenizerFunc()) 
      : first_(first), last_(last), f_(f) { }
        
  /*! \brief Constructs a tokenizer object (without TokenizerFunc object) a
   *         using:
   *
   *  \param c  A string to be used for the token input stream.
   */
  Tokenizer(const Container& c)
      : first_(c.begin()), last_(c.end()), f_() { }
    
  /*! \brief Constructs a tokenizer object using:
   *
   *  \param c  A string to be used for the token input stream.
   *  \param f  TokenizerFunc object for syntax control of token parsing.
   */
  Tokenizer(const Container& c,const TokenizerFunc& f)
      : first_(c.begin()), last_(c.end()), f_(f) { }
  //@}
    
  /*! \brief Method to set the begin and end string::iterator values
   *
   *  \param first String iterator indicating the start of stream to be parsed.
   *  \param last  String iterator indicating the end of stream to be parsed.
   */
  void assign(Iterator first, Iterator last){
    first_ = first;
    last_ = last;
  }
    
  /*! \brief Method to set the begin and end string::iterator values and the
   *         TokenFunc object of this Tokenizer.
   *
   *  \param first String iterator indicating the start of stream to be parsed.
   *  \param last  String iterator indicating the end of stream to be parsed.
   *  \param f     TokenizerFunc object for syntax control of token parsing.
   */
  void assign(Iterator first, Iterator last, const TokenizerFunc& f){
    assign(first,last);
    f_ = f;
  }
    
  /*! \brief Method to set the begin and end string::iterator values of this
   *         Tokenizer.
   *
   *  \param c  A string to be used for the token input stream.
   */
  void assign(const Container& c){
    assign(c.begin(),c.end());
  }
    
  /*! \brief Method to set the begin and end string::iterator values and the
   *         TokenFunc object of this Tokenizer.
   *
   *  \param c  A string to be used for the token input stream.
   *  \param f  TokenizerFunc object for syntax control of token parsing.
   */
  void assign(const Container& c, const TokenizerFunc& f){
    assign(c.begin(),c.end(),f);
  }
    
  //! returns the iterator that points to the start of this object.
  iterator begin() const { return iterator(f_,first_,last_); }

  //! returns the iterator that points to the end of this object.
  iterator end() const { return iterator(f_,last_,last_); }
        
 private:
  //! string::iterator that points to the start of the input stream string
  Iterator first_;

  //! string::iterator that points to the end of the input stream string
  Iterator last_;

  //! Object that knows how to parse the input stream string into tokens
  TokenizerFunc f_;
};

#endif 

