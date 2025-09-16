#include <iostream>
using namespace std;

class paper {
private:
    int matx[100][100] = {0,};

public:
    void fill(int _x, int _y);
    int sum();

    
};


void paper::fill(int _x, int _y){

    int i,j;

    for (j=_y;j<_y+10;j++)
    {
        for (i=_x;i<_x+10;i++)
        {
            
            matx[i][j] = 1;

        }
    }

}


int paper::sum(){

    int i,j,res,row,col;


    row = sizeof(matx)/sizeof(matx[0]);
    col = sizeof(matx[0])/sizeof(matx[0][0]);

    res = 0;

    for (j = 0; j < col; j++)
    {
        for (i = 0; i < row; i++)
        {
            res = res + matx[i][j];
        }
        
    }
    

    return res;


}


int main(void){

    cin.tie(NULL);
	ios_base::sync_with_stdio(false);

    int n,i,x,y,res;
    
    cin>>n;

    paper pap;


    for ( i = 0; i < n; i++){
        
        cin>>x>>y;

        pap.fill(x,y);

    }
    
    res = pap.sum();

    cout<<res;

}