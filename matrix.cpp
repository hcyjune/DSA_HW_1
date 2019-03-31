#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include <new>
#include "matrix.h"

Matrix::Matrix(const int& r, const int& c) //constructor
{
	row=r;
	col=c;
    array=new double* [row];
    for(int i=0;i<row;i++)
    	array[i]=new double[col];
    for(int i=0;i<row;i++)
    	for(int j=0;j<col;j++)
    		array[i][j]=0;
}

Matrix::Matrix(const Matrix& rhs) //copy constructor
{
	row=rhs.row;
	col=rhs.col;
    array=new double* [rhs.row];
    for(int i=0;i<row;i++)
    	array[i]=new double[col];
    for(int i=0;i<rhs.row;i++)
        for(int j=0;j<rhs.col;j++)
            array[i][j]=rhs[i][j];
}

Matrix::~Matrix() //destructor
{
	for(int i=0;i<row;i++)
   		delete [] array[i]; 
 	delete [] array;
}

double* & Matrix::operator [](const int& idx) const //a.array[][] can be represented by a[][]
{
	return array[idx];
}

Matrix Matrix::operator =(const Matrix& rhs) // assignment operator
{
	//if(array!=rhs.array && col==rhs.col && row==rhs.row)
	for(int i=0;i<row;i++)
   		delete [] array[i]; 
 	delete [] array;
	row=rhs.row;
	col=rhs.col;
	array=new double* [row];
    	for(int i=0;i<row;i++)
    		array[i]=new double[col];
    	for(int i=0;i<row;i++)
        	for(int j=0;j<col;j++)
        	    array[i][j]=rhs.array[i][j];	
    return *this;
}

Matrix Matrix::operator -() const //a=-b
{
	Matrix temp(row,col);
	for(int i=0;i<row;i++)
		for(int j=0;j<col;j++)
			temp.array[i][j]=-array[i][j];
	return temp;
}

Matrix Matrix::operator +() const //a=+b
{
	Matrix temp(row,col);
	for(int i=0;i<row;i++)
		for(int j=0;j<col;j++)
			temp.array[i][j]=array[i][j];
	return temp;
}

Matrix Matrix::operator -(const Matrix& rhs) const
{
	Matrix temp(row,col);       
    for(int i=0;i<row;i++)
        for(int j=0;j<col;j++)
            temp.array[i][j]+=array[i][j]-rhs.array[i][j];
    return temp; 
}

Matrix Matrix::operator +(const Matrix& rhs) const
{
	Matrix temp(row,col);     
    for(int i=0;i<row;i++)
        for(int j=0;j<col;j++)
            temp.array[i][j]+=array[i][j]+rhs.array[i][j];
    return temp;
}

Matrix Matrix::operator *(const Matrix& rhs) const
{
    Matrix temp(row,rhs.col);
    for(int i=0;i<row;i++)
        for(int j=0;j<rhs.col;j++)
            for(int k=0;k<col;k++)
                temp[i][j]+=array[i][k]*rhs.array[k][j];
    return temp;
}

Matrix Matrix::operator /(const Matrix& rhs) const
{
	Matrix temp(row,rhs.col);
	Matrix inv=rhs.inverse();
	for(int i=0;i<row;i++)
		for(int j=0;j<inv.col;j++)
			for(int k=0;k<col;k++)
				temp[i][j]+=array[i][k]*inv.array[k][j];
	return temp;
}

Matrix Matrix::inverse() const
{
	//Initialize inverse to identity
	Matrix inverse_matrix(row,col);
	for (int i=0;i<row;i++){
		for (int j=0;j<col;j++)
			if (i==j)
				inverse_matrix[i][j]=1;
			else
				inverse_matrix[i][j]=0;
	}
	//Guass elimination
	double ratio;
	int flag;
	//Get upper triangle
	for (int i=0;i<row-1;i++)
		for (int k=1;k<row-i;k++){
			if (array[i][i]==0){
				flag=0;
				for (int l=i+1;l<row;l++){
					if ((array[l][i]!=0)&&(flag==0)){
						flag=1;
						for (int j=0;j<col;j++){
							array[i][j]+=array[l][j];
							inverse_matrix[i][j]+=inverse_matrix[l][j];
						}	
					}
					if (flag==1)
						break;
				}
			}			
			ratio=array[i+k][i]/array[i][i];
			for (int j=0;j<col;j++){
					array[i+k][j]-=ratio*array[i][j];
					inverse_matrix[i+k][j]-=ratio*inverse_matrix[i][j];
				}
			}
	//Get diagonal form
	for (int i=row-1;i>-1;i--){	
		for (int k=1;k<i+1;k++)
			if (array[i][i]!=0){
			ratio=array[i-k][i]/array[i][i];
			for (int j=0;j<col;j++){
					array[i-k][j]-=ratio*array[i][j];
					inverse_matrix[i-k][j]-=ratio*inverse_matrix[i][j];
				}
		}
	}
	//Normalize digonal term
	for (int i=0;i<row;i++){
		ratio = array[i][i];
		for (int j=0;j<col;j++){
			if (array[i][i]==0)
				printf("No inverse!");
			else
				inverse_matrix[i][j]/=ratio;
		}
	}
	return inverse_matrix;
}

void Matrix::read(const char* fn)
{
	int r, c;
	FILE *fp = fopen(fn, "r");
	if(fp == NULL){
		printf("read file [%s] error\n", fn);
		exit(0);
	}
	fscanf(fp, "%d%d", &r, &c);
	Matrix tmp(r, c);
	for(int i = 0 ; i < r ; i++)
		for(int j = 0 ; j < c ; j++)
			fscanf(fp, "%lf", &tmp.array[i][j]);
	fclose(fp);
	*this = tmp;
}

void Matrix::write(const char* fn)
{
	FILE *fp = fopen(fn, "w");
	if(fp == NULL){
		printf("write file [%s] error\n", fn);
		exit(0);
	}
	fprintf(fp, "%d %d\n", row, col);
	for(int i = 0 ; i < row ; i++)
		for(int j = 0 ; j < col ; j++)
			fprintf(fp, "%lf%c", array[i][j], " \n"[j==col-1]);
	fclose(fp);
}

void Matrix::print() const
{
	for(int i = 0 ; i < row ; i++)
		for(int j = 0 ; j < col ; j++)
			printf("%lf%c", array[i][j], " \n"[j==col-1]);
}
