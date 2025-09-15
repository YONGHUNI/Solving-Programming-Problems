#include <iostream>
using namespace std;


int add(int x, int y){


    return x+y;

}

int main(void){

    cin.tie(NULL);
	ios_base::sync_with_stdio(false);

    int n,x,y,z;
    
    cin>>n;
    


    for (int i=0;i<n;i++){

        cin>>x>>y;
    
        z=add(x,y);

        cout<<z<<"\n";   

    }

}