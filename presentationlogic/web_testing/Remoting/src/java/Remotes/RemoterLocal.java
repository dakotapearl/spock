/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package Remotes;

import javax.ejb.Remote;

/**
 *
 * @author loren
 */
@Remote
public interface RemoterLocal {
    public String getMessage();
}
