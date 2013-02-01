/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package ejbs;


import javax.enterprise.context.RequestScoped;
import javax.inject.Named;

/**
 *
 * @author loren
 */
@Named
@RequestScoped
public class config {

    protected String title = "Spock Web Client";

    /**
     * Get the value of title
     *
     * @return the value of title
     */
    public String getTitle() {
        return title;
    }

    
}
