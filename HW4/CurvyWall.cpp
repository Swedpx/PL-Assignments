#include "CurvyWall.h"

CurvyWall::CurvyWall(float len, float rad) : Wall(len), radius(rad) {center = NULL;}

CurvyWall::CurvyWall(const Particle& beg, const Particle& final, const Particle& cent) : Wall(beg, final){

    center = new Particle(cent);
    radius = start->FindDistance(*center);
    ComputeLength();
}

CurvyWall::CurvyWall(const CurvyWall& rhs) : Wall(rhs) {

    center = new Particle(*rhs.center); 
    radius = rhs.radius;
    ComputeLength();
}

float CurvyWall::GetRadius() const {return radius;}

const Particle& CurvyWall::GetCenter() const{return *center;}

void CurvyWall::ComputeLength(){

    Wall tmp1(*center, *start);
    Wall tmp2(*center, *end);

    double angle = abs(tmp1.FindAngleBetween(tmp2));

    if (*start == *end) length = 2*PI*radius;

    else if (angle < EPSILON)
     
        length = PI*radius;
    
    // else length = abs(tmp1.FindAngleBetween(tmp2))*radius;
    else{
        if (start->X() > end->X()) length = angle*radius;

        else {
            if (start->Y() < end->Y()) length = (2*PI - angle)*radius;
            else length = angle*radius;
        }
    }
}

Wall* CurvyWall::Clone() const{

    CurvyWall* res = new CurvyWall(*start, *end, *center);
    return dynamic_cast<Wall*> (res);
}

bool CurvyWall::IsContinuousLinear(const Wall& rhs) const{return false;}

const Wall& CurvyWall::operator+(const Wall& rhs) const{

    const CurvyWall* test = dynamic_cast<const CurvyWall*>(&rhs);

    if (test == NULL) // rhs is a linear wall
        throw ApplePearException();

    if (*center == *(test->center)){

        if (*end == *test->start){

            CurvyWall* newWall = new CurvyWall(*start, *test->end, *center);
            return *(dynamic_cast<Wall*>(newWall));

        }
        else if (*start == *test->end){
            CurvyWall* newWall = new CurvyWall(*test->start, *end, *center);
            return *(dynamic_cast<Wall*>(newWall));
        }
        else throw ApplePearException();

    } else throw ApplePearException();
}

CurvyWall::~CurvyWall(){delete center;}