/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spock.network.behaviours;

/**
 *
 * @author loren
 */
public interface NetworkRunnable {
    
    /**
     * 
     */
    public void start();
    
    /*
     * 
     */
    public void pauseActivity();
    
    /*
     * 
     */
    public void resumeActivity();
    
}
