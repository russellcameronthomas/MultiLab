/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.gmu.cds.multilab.space;

import sim.util.*;
import edu.gmu.cds.multilab.utilities.Datum;
import edu.gmu.cds.multilab.utilities.Double_nD;

/**
 *
 * @author Russell Thomas
 */
public interface EngageMetricSpace2D {
    
    public void getRadialNeighbors(final int x, final int y, final int dist, int mode, boolean includeOrigin,  Bag result, IntBag xPos, IntBag yPos);

    public void setFocus(Double_nD f);
    public Double_nD getFocus();
    
    public double euclideanDistance(Datum v1, Datum v2);
    
    public double euclideanDistance(Double_nD v1, Double_nD v2);

    public double euclideanDistance(double [] v1, double [] v2);
    
    public double euclideanDistanceSq(Datum v1, Datum v2);
    
    public double euclideanDistanceSq(Double_nD v1, Double_nD v2);

    public double euclideanDistanceSq(double [] v1, double [] v2);
    
    public double manhattanDistance(Datum v1, Datum v2);
    
    public double manhattanDistance(Double_nD v1, Double_nD v2);

    public double manhattanDistance(double [] v1, double [] v2);
    
}
