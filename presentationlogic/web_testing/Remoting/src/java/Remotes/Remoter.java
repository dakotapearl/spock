/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package Remotes;

import javax.ejb.Stateless;

/**
 *
 * @author loren
 */
@Stateless
public class Remoter implements RemoterLocal {

    @Override
    public String getMessage() {
        return "blah";
    }

}
