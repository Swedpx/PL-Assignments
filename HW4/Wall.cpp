#include "Wall.h"
#include "CurvyWall.h"

#define PI 3.14159265

Wall::Wall(float len){ 
    length = len;
    start = NULL;
    end = NULL;
}

Wall::Wall(const Particle& first, const Particle&last){
    start = new Particle(first);
    end = new Particle(last);
    length = start->FindDistance(*end);
}

Wall::Wall(const Wall&rhs){

    start = new Particle(*rhs.start);
    end = new Particle(*rhs.end);
    length = rhs.length;
}

const Particle& Wall::GetInitialParticle() const{ return *start;}
const Particle& Wall::GetFinalParticle() const{ return *end;}

float Wall::FindAngleBetween(const Wall& rhs) const{

    // Aynı noktada başlayıp biten bi wall ?

    double angle_of_rhs;
    double angle_of_this;

    angle_of_this = atan2( (end->Y() - start->Y()) , (end->X() - start->X()));
    angle_of_rhs = atan2((rhs.end->Y() - rhs.start->Y()) , (rhs.end->X() - rhs.start->X()));

    return abs(fmod(angle_of_rhs - angle_of_this, PI));
}

float Wall::GetLength() const{ return length;}

void Wall::ComputeLength(){ length = start->FindDistance(*end);}

Wall* Wall::Clone() const{ return (new Wall(*this));}

bool Wall::IsContinuousLinear(const Wall& rhs) const{

    const CurvyWall* test = dynamic_cast <const CurvyWall*> (&rhs);

    if (test != NULL) return false; // rhs is a CurvyWall

    if (*start == *rhs.end || *end == *rhs.start){
        
        if (abs(FindAngleBetween(rhs)) < EPSILON) return true;
        return false;
    }
    else return false;
}

const Wall& Wall::operator+(const Wall& rhs) const{

    if (IsContinuousLinear(rhs)){

        if (*start == *rhs.end){
            Wall* tmp = new Wall(*rhs.end, *start);
            return *tmp;
        }
        else{
            Wall* tmp = new Wall(*start, *rhs.end);
            return *tmp;
        }

    } else throw ApplePearException();

}

Wall::~Wall(){ delete start; delete end;}