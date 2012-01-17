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

/*! \file StringMatch.h
 *  \brief Interface definitions for the StringMatch class.
 */
 
#ifndef StringMatch_h
#define StringMatch_h 1

#include <list>
#include <string>

class StringMatch
{
 public:
  /*! \brief Constructor
   *
   * \param find Reference to a "match" string. This string supports limited
   *             wildcarding.  The following characters have
   *             special meaning when encountered in the search string:
   *           \li   * is matched by 0 to n characters
   *           \li   ? is matched by exactly 1 character
   *           \li   ^ as a first character anchors the match to the beginning
   *                 of the comment substring
   *           \li   $ as a final character anchors the match to the end
   *                 of the comment substring
   *           \li   \ escapes * and ? to be used literally anywhere in the 
   *                 search string and escapes ^ (and $) at the beginning 
   *                 only (and end only) of the search string to force ^ 
   *                 (or $) to be interpreted literally
   *
   *  \param casSen  If true, matching will be case-sensitive
   */
  StringMatch(const std::string &find, bool caseSen = false);

  //! destructor
  ~StringMatch();

  /*! \brief Method to test a supplied string against the StringMatch object's 
   *         "match" string
   *
   * \param phrase String to test against the "match" string.
   *
   * \return  Returns true if the supplied string matches the "match" string.
   */
  bool IsMatch(const std::string &phrase);

 private:
  //! structure to store a single block of a search specification
  struct ParseBlock
  {
    //! enumeration of block types
    typedef enum {UNDEF=0, LITERAL, FIXED_LEN, VAR_LEN} Type;

    //! constuctor
    ParseBlock();

    //! block type for this parse-block
    Type type;

    /*! \brief length of literal or fixed_length block;
     *         minimum length of a variable_length block
     */
    int len;

    //! for literal block, the literal string to be matched
    std::string literal;
  };

  //! short-hand for a iterator to a list of ParseBlock pointers
  typedef std::list<ParseBlock *>::iterator BlkIterator;

  //! Contains information needed to resume a search from previous location
  struct SavePos {
    //! index of the block in the list of blocks
    int iblk;

    //! iterator to the block in the list
    BlkIterator pos;

    //! Remnant of string to be match at this location in the search
    std::string sc;

    /*! \brief constructor
     *
     *  \param i  Index of block in block list
     *  \param p  Iterator of block in block list
     *  \param s  Remnant of string to be match at this location in the search
     */
    SavePos(int i, BlkIterator p, std::string s) : iblk(i), pos(p), sc(s) { }
  };

  //! work method used to parse the "match" string into a parse-block list
  void parseString();

  //! original match string
  std::string orig;

  //! if true, indicates match must anchor to start of string
  bool beginAnchor;

  //! if true, indicates match must anchor to end of string
  bool endAnchor;

  //! if true, matching is case-sensitive
  bool caseSensitive;

  //! list of parse-block units
  std::list<ParseBlock *> blkList;
};

#endif
