/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package ejbs;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.enterprise.context.SessionScoped;
import javax.inject.Named;

/**
 *
 * @author Loren
 */
@Named
@SessionScoped
public class page implements Serializable {

    protected String PageName;

    /**
     * Get the value of PageName
     *
     * @return the value of PageName
     */
    public String getPageName() {
        return PageName;
    }

    /**
     * Set the value of PageName
     *
     * @param PageName new value of PageName
     */
    public void setPageName(String PageName) {
        this.PageName = PageName;
    }

    public void MovePage(String Page) {
        setPageName(Page);
    }
    
    public page() {
        PageName = "Home";
    }
    
}
