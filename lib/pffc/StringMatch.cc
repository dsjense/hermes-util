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
//  C_Groups

/*! \file StringMatch.cc
 *  \brief Implementation of the StringMatch class.
 */

#include "StringMatch.h"

#include <iostream>
#include <algorithm>
#include <cctype>
#include <cassert>

using std::string;

StringMatch::ParseBlock::ParseBlock()
  : type(UNDEF), len(0), literal("")
{
}

StringMatch::StringMatch(const string &find, bool caseSen)
  : orig(find), beginAnchor(false), endAnchor(false), caseSensitive(caseSen)
{
  parseString();
}

StringMatch::~StringMatch()
{
}

bool StringMatch::IsMatch(const std::string &phrase)
{
  std::list<SavePos> saveList;
  string sc = phrase;

  string::size_type space1 = sc.find_last_not_of(" ") + 1;
  if ( space1 == string::npos ) sc = "";
  string::size_type toss = sc.size() - space1;
  if ( space1 < sc.size() ) sc.erase(space1,toss);

  if ( !caseSensitive ) std::transform(sc.begin(), sc.end(), sc.begin(), tolower);

  bool find = true;
  int nblks = blkList.size();
  BlkIterator pos = blkList.begin();
  ParseBlock *blk = *pos;
  int iblk = 0;
  bool anchored = false;
  while ( find && pos != blkList.end()  ) {
#if 0
    std::cout << "Blk: " << (*pos)->type << " " << (*pos)->len << " \"" << (*pos)->literal
              << "\"" << std::endl;
#endif
    if ( blk->len > (int)sc.size() ) find = false;
    else if ( beginAnchor && iblk == 0 ) {
      // Initial hat (^)
      // if we do not find the string, there is no match
      if ( iblk == nblks-1 && endAnchor && blk->len != (int)sc.size() ) find = false;
      else if ( blk->type ==  ParseBlock::LITERAL ) {
        string::size_type indx = sc.find(blk->literal);
        if ( indx == string::npos || indx != 0 ) find = false;
      }
      anchored = true;
      if ( find ) sc.erase(0,blk->len);
    }
    else if ( iblk == nblks-1 && endAnchor ) {
      if ( anchored && blk->len != (int)sc.size() ) find = false;
      else if ( blk->type ==  ParseBlock::LITERAL ) {
        int indx = (int)sc.size() - blk->len;
        if ( sc.substr(indx) != blk->literal ) find = false;
      }
    }
    else {
      string::size_type indx = 0;
      if ( blk->type ==  ParseBlock::LITERAL ) {
        indx = sc.find(blk->literal);
        if ( indx == string::npos ) find = false;
        else if ( anchored && indx != 0 ) find = false;
        else {
          if ( !anchored ) {
            SavePos save = SavePos(iblk, pos, sc.substr(indx+1));
            saveList.push_back(save);
          }
          anchored = true;
        }
      }
      else if ( blk->type ==  ParseBlock::FIXED_LEN ) anchored = true;
      else anchored = false;
      if ( find ) sc.erase(0,indx + blk->len);
    }
    if ( !find && !saveList.empty() ) {
      SavePos save = saveList.back(); saveList.pop_back();
      pos = save.pos;
      iblk = save.iblk;
      sc = save.sc;
      anchored = false;
#if 0
      std::cout << "Reset to save: " << iblk << " " << sc.size() << " " << sc << std::endl;
      std::cout << "Blk: " << (*pos)->type << " " << (*pos)->len << " \"" << (*pos)->literal
                << "\"" << std::endl;
#endif
      find = true;
    }
    else {
#if 0
      std::cout << anchored << " " << sc.size() << " " << sc << std::endl;
#endif
      ++pos; ++iblk;
    }
    blk = *pos;
  }

  return find;
}

void StringMatch::parseString()
{
#if 0
  std::cout << "\"" << orig << "\"" << std::endl;
#endif
  string::size_type indx = 0;
  indx = orig.find("*?",indx);
  while ( indx != string::npos ) {
    orig.replace(indx,2,"?*");
#if 0
    std::cout << indx << " " << orig << std::endl;
#endif
    indx = orig.find("*?",indx);
  }
  if ( !caseSensitive ) std::transform(orig.begin(), orig.end(), orig.begin(), tolower);
#if 0
  std::cout << "\"" << orig << "\"" << std::endl;
#endif

  const char bslash = '\\';

  string::size_type slen = orig.size();
  if (orig[0] == '^') {
    orig.erase(0,1);
    --slen;
    if (slen == 0 || orig[0] != '*') beginAnchor = true;
  }
  else if ( orig[0] == bslash && orig[1] == '^' ) {
    orig.erase(0,1);
    --slen;
  }
  string::size_type toss = orig.find_first_not_of("*");
  if ( toss == string::npos ) return; // if list empty, everything matches
  if ( toss > 0 ) {
    orig.erase(0,toss);
    slen -= toss;
  }
#if 0
  std::cout << "\"" << orig << "\"" << std::endl;
#endif

  if ( slen && orig[slen-1] == '$' ) {
    if ( slen > 1 && orig[slen-2] == bslash ) orig.erase(slen-2,1);
    else {
      orig.erase(slen-1,1);
      endAnchor = true;
    }
    --slen;
  }

  // remove any trailing blanks and '*'
  // if this includes a '*', then endAnchor can be set to false
  string::size_type space1 = orig.find_last_not_of(" *") + 1;
  if ( space1 == string::npos ) return; // if list empty, everything matches
  string::size_type star = orig.rfind("*");
  if (star != string::npos && star >= space1 ) endAnchor = false;
  toss = slen - space1;
#if 0
  std::cout << "sp1,star,toss: " << space1  << " " << star << " " << toss << std::endl;
#endif
  if ( space1 < slen ) orig.erase(space1,toss);
  slen -= toss;

#if 0
  std::cout << "\"" << orig << "\" " << slen << std::endl;
  std::cout << beginAnchor << " " << endAnchor << std::endl;
#endif

  string::size_type i = 0;
  bool last_esc = false;
  ParseBlock *blk = new ParseBlock;
  while( i < slen ) {
    char c = orig[i];
    if (c == '*') {
      if ( last_esc ) {
        assert(blk->len > 0);
        blk->literal[blk->len-1] = c;
      }
      else if ( blk->type == ParseBlock::LITERAL ) {
        blkList.push_back(blk);
        blk = new ParseBlock;
        blk->type = ParseBlock::VAR_LEN;
      }
      else {
        if ( blkList.empty() ) beginAnchor = false;
        else blk->type = ParseBlock::VAR_LEN;
      }
    }
    else if (c == '?') {
      if ( last_esc ) {
        assert(blk->len > 0);
        blk->literal[blk->len-1] = c;
      }
      else {
        if ( blk->type == ParseBlock::LITERAL ) {
          blkList.push_back(blk);
          blk = new ParseBlock;
          blk->type = ParseBlock::FIXED_LEN;
        }
        else if ( blk->type == ParseBlock::UNDEF ) blk->type = ParseBlock::FIXED_LEN;
        ++blk->len;
      }
    }
    else {
      if ( blk->type != ParseBlock::LITERAL ) {
        if ( blk->type != ParseBlock::UNDEF ) {
          blkList.push_back(blk);
          blk = new ParseBlock;
        }
        blk->type = ParseBlock::LITERAL;
      }
      blk->literal += c;
      ++blk->len;
    }
    if ( c == '\\' ) last_esc = !last_esc;
    else last_esc = false;
    ++i;
  }
  if ( blk->type != ParseBlock::UNDEF ) blkList.push_back(blk);

  //debug
#if 0
  BlkIterator pos = blkList.begin();
  for(int i = 0 ; pos != blkList.end(); ++pos) {
    blk = *pos;
    std::cout << "Block " << i++ <<": "<< blk->type <<" "<< blk->len <<" \""
              << blk->literal << "\"" << std::endl;
  }
#endif
}
