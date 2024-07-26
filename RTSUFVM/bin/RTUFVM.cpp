extern "C" {
  extern void __mained_MOD_parameters();
  extern void __mained_MOD_initialization();
  extern void __mained_MOD_initialconditions();
  extern void __mained_MOD_update(double MagU[]);
  extern void __mained_MOD_saveparaview();
  extern void __mained_MOD_finalization();
}

#include <mpi.h>
#include <cstdio>
//#include <omp.h>


#define AW 101    // Matrix width
#define AH 61     // Matrix height
//#define VW 1920   // Visualization width  (resolution)
//#define VH 1080   // Visualization height (resolution)

/*void normalizarMat2Double(Mat norm, double m[], double min, double max, double ini, double fin, int w, int h){
  double d = 0.0;
  int ind = 0;
  uint8 u;
  for(int i = 0; i < h; i++){
    for(int j = 0; j < w; j++){
      ind = i*w + j;
      m[ind] = abs(m[ind]);
      if(m[ind] <= min) m[ind] = min;
      if(m[ind] >= max) m[ind] = max;

      d = (((m[ind] - min)/(max - min))*(fin - ini)) + ini;
      u = (uint8)d;
      norm.at<uint8>(i, j) = u;
    }
  }
}

  cv::Mat aplicarColorMap(cv::Mat imgray, cv::Mat colormap){
  cv::Mat imcolor;
  int height, width, index;

  height = imgray.rows;
  width  = imgray.cols;
  imcolor = cv::Mat::zeros(height, width, CV_8UC3);

  for(int i = 0; i < height; i++){
    for(int j = 0; j < width; j++){
      index = imgray.at<unsigned char>(i, j);
      imcolor.at<cv::Vec3b>(i, j) = colormap.at<cv::Vec3b>(0, index);
    }
  }

  return imcolor;
}*/

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

  //cv::Mat simul, simulResize;
  //cv::Mat colormap;

  //  _______________________________________________
  // |                                               |
  // |          Inicializacion de variables          |
  // |_______________________________________________|
  
  //-------------------------------------------------  
  //Inicializacion de variables para graficas
  
  //simul    = cv::Mat::zeros(AH, AW, CV_8UC1);
  //colormap = cv::imread("colormaps/oceano.jpg", cv::IMREAD_COLOR);
  // The image oceano.jpg has a resolution of 1 x 255.

  //-------------------------------------------------
  //Inicializacion de variables auxiliares
  
  cont = 0;
  NoSteps = 4000;      // Number of time steps 
  //minMagU = 0.001;
  //maxMagU = 1.25;

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
      if(cont % 2 != 0) continue;
      printf("Contador %i: ejecutado desde C++ \n",cont); 
        __mained_MOD_saveparaview(); 
  }

  //  _______________________________________________
  // |                                               |
  // |       Fin  de las subroutines en ufvm.F       |
  // |_______________________________________________|
  
  __mained_MOD_finalization();
  
  return 0;
}
