extern "C" {
    extern void __mained_MOD_parameters();
    extern void __mained_MOD_initialization();
    extern void __mained_MOD_initialconditions();
    extern void __mained_MOD_update(double MagU[]);
    extern void __mained_MOD_saveparaview();
    extern void __mained_MOD_readviscosity();
    extern void __mained_MOD_finalization();
}

#include <mpi.h>
#include <cstdio>

#define AW 101    // Matrix width
#define AH 61     // Matrix height

int main(int argc, char *argv[] ){
        //  _______________________________________________
    // |                                               |
    // |            Definicion de variables            |
    // |_______________________________________________|

    double MagU[AW*AH];
    double minMagU;
    double maxMagU;
  
    int c;
    int cont;
    int NoSteps;

    //  _______________________________________________
    // |                                               |
    // |          Inicializacion de variables          |
    // |_______________________________________________|
  
  
    cont = 0;
    NoSteps = 400000;      // Number of time steps 

    //  _______________________________________________
    // |                                               |
    // |     Inicio de las subroutines en ufvm.F       |
    // |_______________________________________________|
     
    __mained_MOD_parameters();
    __mained_MOD_initialization();
    __mained_MOD_initialconditions();

    //  _______________________________________________
    // |                                               |
    // |     Loop con actualizaciones y despliegue     |
    // |_______________________________________________|


    for(;cont < NoSteps; ++cont){
        __mained_MOD_update(MagU);
        if(cont % 4 != 0) continue;
        printf("Contador %i: ejecutado desde C++ \n",cont);
        __mained_MOD_readviscosity();
        __mained_MOD_saveparaview(); 
    }

    //  _______________________________________________
    // |                                               |
    // |       Fin  de las subroutines en ufvm.F       |
    // |_______________________________________________|
  
    __mained_MOD_finalization();
  
    return 0;
}


