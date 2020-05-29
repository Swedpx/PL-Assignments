#include "Particle.h"

Particle::Particle(float nx, float ny) : x(nx), y(ny){}

Particle::Particle(const Particle& rhs) : x(rhs.x), y(rhs.y){}

float Particle::X() const{ return x;}
float Particle::Y() const{ return y;}

float Particle::FindDistance(const Particle& rhs) const{
    return sqrt((pow((rhs.x - x), 2.0) + pow((rhs.y - y), 2.0)));
}

bool Particle::operator==(const Particle& rhs) const{

    return FindDistance(rhs) < EPSILON;
}

ostream& operator<<(ostream& os, const Particle& part){

    os << "(" << part.X() << "," << part.Y() << ")";
    return os;
}