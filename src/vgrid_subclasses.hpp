// libdescrip - Vertical grid descriptor library for FORTRAN programming
// Copyright (C) 2016  Direction du developpement des previsions nationales
//                     Centre meteorologique canadien
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef VGRID_5005_H
#define VGRID_5005_H

#include "vgrid.hpp"

class vgrid_0001 : public vgrid
{
public:
  vgrid_0001(int key);
  int c_decode_vert();
};

class vgrid_1001 : public vgrid
{
public:
  vgrid_1001(int key);
  int c_decode_vert();
};

class vgrid_1002 : public vgrid
{
public:
  vgrid_1002(int key);
  int c_decode_vert();
};

class vgrid_1003_5001 : public vgrid
{
public:
  vgrid_1003_5001(int key);
  int c_decode_vert();
};

class vgrid_2001 : public vgrid
{
public:
  vgrid_2001(int key);
  int c_decode_vert();
};

class vgrid_4001 : public vgrid
{
public:
  vgrid_4001(int key);
  int c_decode_vert();
};

class vgrid_5001 : public vgrid_1003_5001
{
public:
  vgrid_5001(int key);
};

class vgrid_5002_5003_5004_5005 : public vgrid
{
public:
  vgrid_5002_5003_5004_5005(int key, int k_plus_top_value);
  int c_decode_vert();
private:
  int k_plus_top;
};

class vgrid_5002 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5002(int key);
};

class vgrid_5003 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5003(int key);
};

class vgrid_5004 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5004(int key);
};

class vgrid_5005 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5005(int key);
};

class vgrid_5100 : public vgrid
{
public:
  vgrid_5100(int key);
  int c_decode_vert();
};

class vgrid_5999 : public vgrid
{
public:
  vgrid_5999(int key);
  int c_decode_vert();
};

class vgrid_21001 : public vgrid
{
public:
  vgrid_21001(int key);
  int c_decode_vert();
};

class vgrid_21002 : public vgrid
{
public:
  vgrid_21002(int key);
  int c_decode_vert();
};

#endif // VGRID_5005_H
