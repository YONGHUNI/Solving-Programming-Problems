#include <iostream>
using namespace std;


int mul(int x, int y){

    return x*y;

}

int main(void){

    int x,y,z;
    
    cin>>x>>y;
    
    z=mul(x,y);

    cout<<z;

}